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

const LiteralCharTokens = ["{", "}", "[", "]", "=", '"', "."] as const;
type LiteralCharToken = (typeof LiteralCharTokens)[number];

const LiteralStringTokens = [
  "..",
  "let",
  "in",
  "true",
  "false",
  "null",
] as const;
type LiteralStringToken = (typeof LiteralStringTokens)[number];

export type Token =
  | { type: LiteralCharToken }
  | { type: LiteralStringToken }
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
      createMatcher(this.matchCharLiteral("{"), {
        action: "push",
        state: State.Object,
      }),
      createMatcher(this.matchStringLiteral("let"), {
        action: "push",
        state: State.AssignBlock,
      }),
    ],
    [State.AssignBlock]: [
      createMatcher(this.matchStringLiteral("in"), { action: "pop" }),

      createMatcher(this.matchCharLiteral("{")),
      createMatcher(this.matchCharLiteral("}")),
      createMatcher(this.matchInput.bind(this)),
      createMatcher(this.matchCharLiteral("="), {
        action: "push",
        state: State.Value,
      }),
    ],
    [State.Value]: [
      createMatcher(this.matchCharLiteral("{"), {
        action: "replace",
        state: State.Object,
      }),
      createMatcher(this.matchCharLiteral("["), {
        action: "replace",
        state: State.Array,
      }),

      createMatcher(this.matchStringLiteral("true"), { action: "pop" }, [...WHITESPACE, '}']),
      createMatcher(this.matchStringLiteral("false"), { action: "pop" }, [...WHITESPACE, '}']),
      createMatcher(this.matchStringLiteral("null"), { action: "pop" }, [...WHITESPACE, '}']),

      createMatcher(this.matchCharLiteral('"'), {
        action: "replace",
        state: State.String,
      }),

      createMatcher(this.matchInput.bind(this), { action: "pop" }, [...WHITESPACE, '}']),
      createMatcher(this.matchFloat.bind(this), { action: "pop" }, [...WHITESPACE, '}']),
      createMatcher(this.matchInteger.bind(this), { action: "pop" }, [...WHITESPACE, '}']),

      createMatcher(this.matchCharLiteral("]"), { action: "pop" }),
    ],
    [State.Object]: [
      createMatcher(this.matchCharLiteral("}"), { action: "pop" }),

      createMatcher(this.matchCharLiteral("="), {
        action: "push",
        state: State.Value,
      }),
      createMatcher(this.matchStringLiteral("..")),
      createMatcher(this.matchCharLiteral(".")),
      createMatcher(this.matchInput.bind(this)),
      createMatcher(this.matchQuotedPathSegment.bind(this)),
      createMatcher(this.matchPathSegment.bind(this)),
    ],
    [State.Array]: [
      createMatcher(this.matchCharLiteral("]"), { action: "pop" }),

      createMatcher(this.matchCharLiteral("{"), {
        action: "push",
        state: State.Object,
      }),
      createMatcher(this.matchCharLiteral("["), {
        action: "push",
        state: State.Array,
      }),
      createMatcher(this.matchCharLiteral('"'), {
        action: "push",
        state: State.String,
      }),

      createMatcher(this.matchStringLiteral("..")),

      createMatcher(this.matchStringLiteral("true"), undefined, [...WHITESPACE, ']']),
      createMatcher(this.matchStringLiteral("false"), undefined, [...WHITESPACE, ']']),
      createMatcher(this.matchStringLiteral("null"), undefined, [...WHITESPACE, ']']),

      createMatcher(this.matchInput.bind(this), undefined, [...WHITESPACE, ']']),
      createMatcher(this.matchFloat.bind(this), undefined, [...WHITESPACE, ']']),
      createMatcher(this.matchInteger.bind(this), undefined, [...WHITESPACE, ']']),
    ],
    [State.String]: [
      createMatcher(this.matchCharLiteral('"'), { action: "pop" }),

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

    let state: State[] = [State.TopLevel];
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

      if (currentState !== State.String && char === "/" && input[1] === "/") {
        input.splice(0, 2);
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

        if(matcher.requiredChars.length) {
          const nextChar = input[0];
          if(!matcher.requiredChars.includes(nextChar)) {
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

    const [baseA, baseB] = input;

    let base = 10;
    let matcher = isDigit;
    if (baseA === "0") {
      switch (baseB) {
        case "b":
          base = 2;
          matcher = (char: string) => char === "0" || char === "1";
          break;
        case "x":
          base = 16;
          matcher = isHexDigit;
          break;
        case "o":
          base = 8;
          matcher = (char: string) => char >= "0" && char <= "7";
          break;
      }
    }

    if (base !== 10) input.splice(0, 2);

    const value: string[] = [];
    let char = input[0];
    while(char && (matcher(char) || char === "_")) {
      const match = takeWhile(input, matcher);
      if (!match.length) return ok(undefined);
      value.push(...match);

      char = input[0];
      if(char === "_") {
        input.shift();
      }
    }

    if (value.length) {
      let num = Number.parseInt(value.join(""), base);

      if(!Number.isSafeInteger(num)) {
        return err({
          message: "integer out of range",
          span: this.getAbsoluteSpan({
            startCol: 0,
            endCol: value.length,
            endRow: 0,
          }),
        });
      }

      if (Number.isNaN(num)) return err({
        message: "invalid integer",
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: value.length,
          endRow: 0,
        }),
      })

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

  // TODO: Rewrite - this uses far too much string building
  private matchFloat(input: string[]): TokenResult {
    if (input.length < 2)
      return err({
        message: "unexpected end of input",
        span: this.getAbsoluteSpan({
          startCol: 0,
          endCol: 1,
          endRow: 0,
        }),
      });

    const sign = input[0];
    const isNegative = sign === "-";

    const intPartChars: string[] = [];

    // we don't want to take from input yet
    // as this may be an integer rather than a float
    let i = isNegative ? 1 : 0;
    let char = input[i];

    while (isDigit(char)) {
      intPartChars.push(char);
      char = input[++i];
    }

    const intPart = Number.parseInt(intPartChars.join(""), 10);
    if (Number.isNaN(intPart) || char !== ".") return ok(undefined);

    input.splice(0, i + 1);

    const floatPartChars = takeWhile(input, isDigit);

    let exponent = "";
    if (input[0] === "e") {
      exponent += input.shift();

      const expSign = input[0] as string; // ts things this is "e"
      if (expSign !== "+" && expSign !== "-") {
        return err({
          message: "expected one of `+` or `-`",
          span: this.getAbsoluteSpan({
            startCol: intPartChars.length + floatPartChars.length + 1,
            endCol: intPartChars.length + floatPartChars.length + 2,
            endRow: 0,
          }),
        });
      }

      exponent += input.shift();

      const expPartChars = takeWhile(input, isDigit);

      let expPart = Number.parseInt(expPartChars.join(""), 10);
      if (Number.isNaN(expPart)) {
        return err({
          message: "float exponent must be a base 10 integer",
          span: this.getAbsoluteSpan({
            startCol: intPartChars.length + floatPartChars.length + 2,
            endCol: intPartChars.length + floatPartChars.length + 3,
            endRow: 0,
          }),
        });
      }

      exponent += expPart.toString();
    }

    const fullString = `${intPart}.${floatPartChars.join("")}${exponent}`;
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
        char !== '"' && char !== "\\" && (char != "$" || next !== "{"),
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

  private matchCharLiteral(char: LiteralCharToken) {
    return (input: string[]): TokenResult => {
      if (input[0] === char) {
        input.shift();
        return ok({
          token: { type: char },
          span: this.getAbsoluteSpan({ startCol: 0, endCol: 1, endRow: 0 }),
        });
      }

      return ok(undefined);
    };
  }

  private matchStringLiteral(str: LiteralStringToken) {
    return (input: string[]): TokenResult => {
      if (input.slice(0, str.length).join("") === str) {
        input.splice(0, str.length);
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

function createMatcher(func: MatcherFunc, stateChange?: StateChange, requiredChars: string[] = []): Matcher {
  return {
    func,
    stateChange: stateChange ?? { action: "none" },
    requiredChars
  };
}

/**
 * Attempts to take `char` from the start of `input`.
 * If `char` is not present, returns `false` and does nothing.
 *
 * If `char` is present, it is removed from the front of `input`,
 * and returns `true`.
 * @param input
 * @param chars
 */
function take(input: string[], ...chars: string[]): boolean {
  for (let i = 0; i < chars.length; i++) {
    if (input[i] !== chars[i]) return false;
  }

  input.splice(0, chars.length);
  return true;
}

function takeWhile(
  input: string[],
  predicate: (char: string, next: string | undefined) => boolean,
) {
  const result: string[] = [];
  while (predicate(input[0], input[1]) && input.length > 0) {
    result.push(input.shift() as string);
  }
  return result;
}
