import {
  assertNever,
  err,
  isAlphanumeric,
  isDigit,
  isHexDigit,
  isLetter,
  ok,
  Result,
} from "./utils";

const WHITESPACE = "\n\r\t ";

type LiteralChar = "{" | "}" | "[" | "]" | "=" | '"' | ".";
type LiteralString = ".." | "let" | "in" | "true" | "false" | "null";

export type Token =
  | { type: LiteralChar }
  | { type: LiteralString }
  | { type: "pathSegment"; value: string }
  | { type: "integer"; value: number }
  | { type: "float"; value: number }
  | { type: "input"; value: string }
  | { type: "charSequence"; value: string }
  | { type: "charEscape"; value: string }
  | { type: "unicodeEscape"; value: number }
  | { type: "null" };

const enum State {
  TopLevel,
  AssignBlock,
  Value,
  Object,
  Array,
  String,
}

type StateChange =
  | { action: "push"; state: State }
  | { action: "replace"; state: State }
  | { action: "pop" }
  | { action: "none" };

export type LexerError = {
  message: string;
  span: Span;
};

export type TokenWithSpan = {
  token: Token;
  span: Span;
};

export type Span = {
  startRow: number;
  endRow: number;
  startCol: number;
  endCol: number;
};

type RelativeSpan = {
  startCol: number;
  endCol: number;
  endRow: number;
};

type TokenResult = Result<TokenWithSpan | undefined, LexerError>;
type MatcherFunc = (input: string[]) => TokenResult;
type Matcher = {
  func: MatcherFunc;
  stateChange: StateChange;
  requiredChars: string[];
};

export class Lexer {
  private row = 0;
  private col = 0;

  private readonly MATCHERS: Record<State, Matcher[]> = {
    [State.TopLevel]: [
      createMatcher(this.matchLiteral("{"), {
        action: "push",
        state: State.Object,
      }),
      createMatcher(this.matchLiteral("let"), {
        action: "push",
        state: State.AssignBlock,
      }),
    ],
    [State.AssignBlock]: [
      createMatcher(this.matchLiteral("in"), { action: "pop" }),

      createMatcher(this.matchLiteral("{")),
      createMatcher(this.matchLiteral("}")),
      createMatcher(this.matchInput.bind(this)),
      createMatcher(this.matchLiteral("="), {
        action: "push",
        state: State.Value,
      }),
    ],
    [State.Value]: [
      createMatcher(this.matchLiteral("{"), {
        action: "replace",
        state: State.Object,
      }),
      createMatcher(this.matchLiteral("["), {
        action: "replace",
        state: State.Array,
      }),

      createMatcher(this.matchLiteral("true"), { action: "pop" }, [
        ...WHITESPACE,
        "}",
      ]),
      createMatcher(this.matchLiteral("false"), { action: "pop" }, [
        ...WHITESPACE,
        "}",
      ]),
      createMatcher(this.matchLiteral("null"), { action: "pop" }, [
        ...WHITESPACE,
        "}",
      ]),

      createMatcher(this.matchLiteral('"'), {
        action: "replace",
        state: State.String,
      }),

      createMatcher(this.matchInput.bind(this), { action: "pop" }, [
        ...WHITESPACE,
        "}",
      ]),
      createMatcher(this.matchFloat.bind(this), { action: "pop" }, [
        ...WHITESPACE,
        "}",
      ]),
      createMatcher(this.matchInteger.bind(this), { action: "pop" }, [
        ...WHITESPACE,
        "}",
      ]),

      createMatcher(this.matchLiteral("]"), { action: "pop" }),
    ],
    [State.Object]: [
      createMatcher(this.matchLiteral("}"), { action: "pop" }),

      createMatcher(this.matchLiteral("="), {
        action: "push",
        state: State.Value,
      }),
      createMatcher(this.matchLiteral("..")),
      createMatcher(this.matchLiteral(".")),
      createMatcher(this.matchInput.bind(this)),
      createMatcher(this.matchQuotedPathSegment.bind(this)),
      createMatcher(this.matchPathSegment.bind(this)),
    ],
    [State.Array]: [
      createMatcher(this.matchLiteral("]"), { action: "pop" }),

      createMatcher(this.matchLiteral("{"), {
        action: "push",
        state: State.Object,
      }),
      createMatcher(this.matchLiteral("["), {
        action: "push",
        state: State.Array,
      }),
      createMatcher(this.matchLiteral('"'), {
        action: "push",
        state: State.String,
      }),

      createMatcher(this.matchLiteral("..")),

      createMatcher(this.matchLiteral("true"), undefined, [...WHITESPACE, "]"]),
      createMatcher(this.matchLiteral("false"), undefined, [
        ...WHITESPACE,
        "]",
      ]),
      createMatcher(this.matchLiteral("null"), undefined, [...WHITESPACE, "]"]),

      createMatcher(this.matchInput.bind(this), undefined, [
        ...WHITESPACE,
        "]",
      ]),
      createMatcher(this.matchFloat.bind(this), undefined, [
        ...WHITESPACE,
        "]",
      ]),
      createMatcher(this.matchInteger.bind(this), undefined, [
        ...WHITESPACE,
        "]",
      ]),
    ],
    [State.String]: [
      createMatcher(this.matchLiteral('"'), { action: "pop" }),

      createMatcher(this.matchInterpolatedInput.bind(this)),
      createMatcher(this.matchCharEscape.bind(this)),
      createMatcher(this.matchCharSequence.bind(this)),
    ],
  };

  public tokenizeInput(
    inputString: string,
  ): Result<TokenWithSpan[], LexerError> {
    const input = inputString.split("");

    const tokens: TokenWithSpan[] = [];

    const state: State[] = [State.TopLevel];
    while (input.length > 0) {
      const currLength = input.length;
      const currentState = state[state.length - 1];

      const char = input[0];
      if (!char)
        return err({
          message: "lexer error - expected char",
          span: this.getAbsoluteSpan({ startCol: 0, endCol: 0, endRow: 0 }),
        });

      if (currentState !== State.String && WHITESPACE.includes(char)) {
        if (char === "\n") {
          this.row++;
          this.col = 0;
        } else {
          this.col++;
        }

        input.shift();
        continue;
      }

      if (currentState !== State.String && take(input, "/", "/")) {
        takeWhile(input, (char) => char !== "\n");
        continue;
      }

      const matchers = this.MATCHERS[currentState];

      let hasMatch = false;
      for (const matcher of matchers) {
        const match = matcher.func(input);

        if (!match.ok) return match;
        if (!match.value) continue;
        hasMatch = true;

        tokens.push(match.value);
        this.setSpan(match.value.span);

        switch (matcher.stateChange.action) {
          case "push":
            state.push(matcher.stateChange.state);
            break;
          case "replace":
            state[state.length - 1] = matcher.stateChange.state;
            break;
          case "pop":
            state.pop();
            break;
          case "none":
            break;
          default:
            assertNever(matcher.stateChange);
        }

        // some tokens cannot be next to other tokens
        // (for example, to prevent `[truefalsenull]` in an array).
        if (matcher.requiredChars.length) {
          const nextChar = input[0];
          if (!matcher.requiredChars.includes(nextChar)) {
            return err({
              message: `expected whitespace after ${match.value.token.type}`,
              span: match.value.span,
            });
          }
        }

        break;
      }

      if (!hasMatch) {
        return err({
          message: `Unexpected token: ${char}`,
          span: this.getAbsoluteSpan({
            startCol: 0,
            endCol: 1,
            endRow: 0,
          }),
        });
      }

      if (input.length === currLength) {
        throw new Error(
          `no match for char ${char} in state ${currentState} - input length is unchanged. This is a lexer bug!`,
        );
      }
    }

    return ok(tokens);
  }

  private matchPathSegment(input: string[]): TokenResult {
    const value = takeWhile(
      input,
      (char) =>
        !WHITESPACE.includes(char) &&
        char !== "." &&
        char !== "=" &&
        char !== "'" &&
        char !== '"',
    );

    if (value.length) {
      const token: Token = { type: "pathSegment", value: value.join("") };
      return ok({
        token,
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: value.length,
          endRow: 0,
        }),
      });
    }

    return ok(undefined);
  }

  private matchQuotedPathSegment(input: string[]): TokenResult {
    if (!take(input, "'")) return ok(undefined);

    let escaping = false;
    const value = takeWhile(input, (char) => {
      if (char === "\\") escaping = true;

      if (char === "'" && !escaping) return false;
      if (char === "'" && escaping) {
        escaping = false;
        return true;
      }

      return char !== "'";
    });

    if (!take(input, "'")) {
      return err({
        message: "expected closing quote",
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: value.length,
          endRow: 0,
        }),
      });
    }

    const token: Token = {
      type: "pathSegment",
      value: value.filter((c) => c !== "\\").join(""),
    };

    return ok({
      token,
      span: this.getAbsoluteSpan({
        startCol: 0,
        endCol: value.length + 1,
        endRow: 0,
      }),
    });
  }

  private matchInteger(input: string[]): TokenResult {
    if (input.length < 2)
      return err({
        message: "unexpected end of input",
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: 1,
          endRow: 0,
        }),
      });

    const isNegative = take(input, "-");

    let base = 10;
    let matcher = isDigit;
    switch (true) {
      case !!take(input, "0", "b"):
        base = 2;
        matcher = (char: string) => char === "0" || char === "1";
        break;
      case !!take(input, "0", "x"):
        base = 16;
        matcher = isHexDigit;
        break;
      case !!take(input, "0", "o"):
        base = 8;
        matcher = (char: string) => char >= "0" && char <= "7";
        break;
    }

    const value: string[] = [];
    let char = input[0];
    while (char && (matcher(char) || char === "_")) {
      const match = takeWhile(input, matcher);
      if (!match.length) return ok(undefined);
      value.push(...match);

      char = input[0];
      take(input, "_");
    }

    if (value.length) {
      let num = Number.parseInt(value.join(""), base);

      if (!Number.isSafeInteger(num)) {
        return err({
          message: "integer out of range",
          span: this.getAbsoluteSpan({
            startCol: 0,
            endCol: value.length,
            endRow: 0,
          }),
        });
      }

      if (Number.isNaN(num))
        return err({
          message: "invalid integer",
          span: this.getAbsoluteSpan({
            startCol: 0,
            endCol: value.length,
            endRow: 0,
          }),
        });

      if (isNegative) num *= -1;
      const token: Token = { type: "integer", value: num };
      return ok({
        token,
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: value.length,
          endRow: 0,
        }),
      });
    }

    return ok(undefined);
  }

  private matchFloat(input: string[]): TokenResult {
    if (input.length < 2) {
      return err({
        message: "unexpected end of input",
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: 1,
          endRow: 0,
        }),
      });
    }

    const isNegative = peek(input, "-");
    const negOffset = isNegative ? 1 : 0;
    const intPartChars: string[] = peekWhile(input.slice(negOffset), isDigit);

    if (input[intPartChars.length + negOffset] !== ".") return ok(undefined);

    input.splice(0, intPartChars.length + negOffset + 1);

    const floatPartChars = takeWhile(input, isDigit);

    let exponent = "";
    if (take(input, "e")) {
      exponent += "e";

      const expSign = take(input, "+") || take(input, "-");
      if (!expSign) {
        return err({
          message: "expected one of `+` or `-`",
          span: this.getAbsoluteSpan({
            startCol: intPartChars.length + floatPartChars.length + 1,
            endCol: intPartChars.length + floatPartChars.length + 2,
            endRow: 0,
          }),
        });
      }

      exponent += expSign;

      const expPartChars = takeWhile(input, isDigit);
      exponent += expPartChars.join("");
    }

    const fullString = `${intPartChars.join("")}.${floatPartChars.join("")}${exponent}`;
    let num = parseFloat(fullString);
    if (Number.isNaN(num)) {
      return err({
        message: "invalid float",
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: fullString.length,
          endRow: 0,
        }),
      });
    }

    if (isNegative) num *= -1;
    const token: Token = { type: "float", value: num };
    return ok({
      token,
      span: this.getAbsoluteSpan({
        startCol: 0,
        endCol: fullString.length,
        endRow: 0,
      }),
    });
  }

  private matchInterpolatedInput(input: string[]): TokenResult {
    if (!take(input, "$", "{")) return ok(undefined);

    const name =
      "$" +
      takeWhile(input, (char) => isAlphanumeric(char) || char === "_").join("");

    if (!take(input, "}")) return ok(undefined);

    const token: Token = { type: "input", value: name };
    return ok({
      token,
      span: this.getAbsoluteSpan({
        startCol: 0,
        endCol: name.length + 2,
        endRow: 0,
      }),
    });
  }

  private matchCharEscape(input: string[]): TokenResult {
    if (!take(input, "\\")) return ok(undefined);

    const span = this.getAbsoluteSpan({ startCol: 0, endCol: 2, endRow: 0 });
    switch (input.shift()) {
      case "n":
        return ok({ token: { type: "charEscape", value: "\n" }, span });
      case "r":
        return ok({ token: { type: "charEscape", value: "\r" }, span });
      case "t":
        return ok({ token: { type: "charEscape", value: "\t" }, span });
      case "\\":
        return ok({ token: { type: "charEscape", value: "\\" }, span });
      case '"':
        return ok({ token: { type: "charEscape", value: '"' }, span });
      case "$":
        return ok({ token: { type: "charEscape", value: "$" }, span });
      case "u":
        return this.matchUnicodeEscape(input);
      default:
        return err({ message: "invalid char escape", span });
    }
  }

  private matchUnicodeEscape(input: string[]): TokenResult {
    // since this runs effectively after charEscape,
    // we only need to start matching from the brace.
    const SPAN_ROW_OFFSET = 2;

    if (!take(input, "{")) {
      return err({
        message: "expected {",
        span: this.getAbsoluteSpan({
          startCol: SPAN_ROW_OFFSET,
          endCol: SPAN_ROW_OFFSET + 1,
          endRow: 0,
        }),
      });
    }

    const hex = takeWhile(input, isHexDigit);

    if (!take(input, "}")) {
      return err({
        message: "expected }",
        span: this.getAbsoluteSpan({
          startCol: SPAN_ROW_OFFSET + hex.length,
          endCol: SPAN_ROW_OFFSET + hex.length + 1,
          endRow: 0,
        }),
      });
    }

    if (!hex.length) {
      return err({
        message: "unicode escape cannot be empty",
        span: this.getAbsoluteSpan({
          startCol: SPAN_ROW_OFFSET,
          endCol: SPAN_ROW_OFFSET + 2,
          endRow: 0,
        }),
      });
    }

    const value = parseInt(hex.join(""), 16);

    return ok({
      token: { type: "unicodeEscape", value },
      span: this.getAbsoluteSpan({
        startCol: 0,
        endCol: hex.length + 4,
        endRow: 0,
      }),
    });
  }

  private matchCharSequence(input: string[]): TokenResult {
    const value = takeWhile(
      input,
      (char, next) =>
        char !== '"' && char !== "\\" && (char !== "$" || next !== "{"),
    );

    if (value.length) {
      const rows = value.filter((c) => c === "\n").length;
      const lastColumnIndex = value.lastIndexOf("\n") + 1;
      const endColumn = value.length - lastColumnIndex;

      const token: Token = { type: "charSequence", value: value.join("") };
      return ok({
        token: token,
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: endColumn,
          endRow: rows,
        }),
      });
    }

    return ok(undefined);
  }

  private matchInput(input: string[]): TokenResult {
    if (!take(input, "$")) return ok(undefined);

    if (!isLetter(input[0]) && input[0] !== ("_" as string))
      return ok(undefined);

    const name =
      "$" +
      takeWhile(input, (char) => isAlphanumeric(char) || char === "_").join("");

    const token: Token = { type: "input", value: name };
    return ok({
      token,
      span: this.getAbsoluteSpan({
        startCol: 0,
        endCol: name.length,
        endRow: 0,
      }),
    });
  }

  /**
   * Returns a matcher function for a literal token.

   * @param str Literal token value.
   * @private
   */
  private matchLiteral(str: LiteralChar | LiteralString) {
    return (input: string[]): TokenResult => {
      if (take(input, ...str)) {
        return ok({
          token: { type: str },
          span: this.getAbsoluteSpan({
            startCol: 0,
            endCol: str.length,
            endRow: 0,
          }),
        });
      }

      return ok(undefined);
    };
  }

  /**
   * Applies a relative span to the current row/col
   * to get an absolute span.
   *
   * @param relativeSpan
   * @private
   */
  private getAbsoluteSpan(relativeSpan: RelativeSpan): Span {
    const endCol = relativeSpan.endRow > 1 ? 0 : this.col;
    return {
      startCol: this.col + relativeSpan.startCol,
      startRow: this.row,
      endCol: endCol + relativeSpan.endCol,
      endRow: this.row + relativeSpan.endRow,
    };
  }

  private setSpan(span: Span) {
    this.row = span.endRow;
    this.col = span.endCol;
  }
}

/**
 * Creates a new matcher object.
 * @param func
 * @param stateChange
 * @param requiredChars
 */
function createMatcher(
  func: MatcherFunc,
  stateChange?: StateChange,
  requiredChars: string[] = [],
): Matcher {
  return {
    func,
    stateChange: stateChange ?? { action: "none" },
    requiredChars,
  };
}

/**
 * Checks if the provided chars are present at the start of the input.
 * Characters are not consumed.
 *
 * @param input Input chars
 * @param chars Check chars, in order.
 * @returns Whether chars exist at start of input.
 */
function peek(input: string[], ...chars: string[]): boolean {
  for (let i = 0; i < chars.length; i++) {
    if (input[i] !== chars[i]) return false;
  }

  return true;
}

/**
 * Like {@link takeWhile},
 * but does not take the characters from `input`.
 *
 * @param input Input chars
 * @param predicate Check function.
 */
function peekWhile(
  input: string[],
  predicate: (char: string, next: string | undefined) => boolean,
): string[] {
  const result: string[] = [];

  let char = input[0];
  let i = 0;
  while (predicate(input[i], input[i + 1]) && i < input.length) {
    result.push(char);
    char = input[++i];
  }

  return result;
}

/**
 * Attempts to take `chars` from the start of `input`.
 * If any of `chars` is not present, returns `false` and does nothing.
 *
 * If all `chars` are present, they are removed from the front of `input`
 * and returned.
 *
 * @param input Input chars
 * @param chars chars to take, in order
 * @returns taken chars, or `false`.
 */
function take(input: string[], ...chars: string[]): string[] | false {
  const res = peek(input, ...chars);
  if (res) input.splice(0, chars.length);
  return res ? chars : false;
}

/**
 * Takes chars from the start of `input`
 * for as long as `predicate` is true.
 *
 * The predicate function takes the current and next char,
 * and returns a boolean.
 *
 * @param input Input chars.
 * @param predicate Check function.
 */
function takeWhile(
  input: string[],
  predicate: (char: string, next: string | undefined) => boolean,
) {
  const res = peekWhile(input, predicate);
  if (res.length) input.splice(0, res.length);
  return res;
}
