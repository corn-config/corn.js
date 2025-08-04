import { Token } from "./lexer";
import { err, ok, Result } from "./utils";

type TokenType = Token["type"];

export type RuleConfig = {
  type: "config";
  assignBlock?: RuleAssignBlock;
  object: RuleObject;
};

export type RuleAssignBlock = {
  type: "assignBlock";
  assignments: RuleAssignment[];
};
export type RuleAssignment = {
  type: "assignment";
  key: string;
  value: RuleValue;
};

export type RuleInput = { type: "input"; value: string };
export type RuleBoolean = { type: "boolean"; value: boolean };
export type RuleInteger = { type: "integer"; value: number };
export type RuleFloat = { type: "float"; value: number };
export type RuleString = { type: "string"; value: RuleStringPart[] };
export type RuleNull = { type: "null" };

export type RuleArray = { type: "array"; values: RuleArrayItem[] };

export type RuleObject = { type: "object"; pairs: RuleObjectItem[] };
export type RulePair = { type: "pair"; path: RulePath; value: RuleValue };
export type RulePath = { type: "path"; value: string[] };
export type RuleSpread = { type: "spread"; value: string };

export type RuleCharSequence = { type: "charSequence"; value: string };
export type RuleCharEscape = { type: "charEscape"; value: string };
export type RuleUnicodeEscape = { type: "unicodeEscape"; value: number };

export type RuleValue =
  | RuleInput
  | RuleBoolean
  | RuleInteger
  | RuleFloat
  | RuleString
  | RuleArray
  | RuleObject
  | RuleNull;

export type RuleObjectItem = RulePair | RuleSpread;
export type RuleArrayItem = RuleValue | RuleSpread;

export type RuleStringPart =
  | RuleCharSequence
  | RuleCharEscape
  | RuleUnicodeEscape
  | RuleInput;

/**
 * Attempts to parse the provided token array
 * into an AST of rules.
 *
 * @param tokens The token array.
 * @returns A result containing the AST if successfully parsed,
 * or an error message if not.
 */
export function parseTokens(tokens: Token[]): Result<RuleConfig, string> {
  if (tokens.length === 0) return err("no tokens");

  const token = tokens[0];

  let assignBlock;
  if (token.type === "let") {
    assignBlock = parseAssignBlock(tokens);
    if (!assignBlock.ok) return assignBlock;
  }

  const object = parseObject(tokens);
  if (!object.ok) return object;

  return ok({
    type: "config",
    assignBlock: assignBlock?.value,
    object: object.value,
  } as RuleConfig);
}

function parseAssignBlock(tokens: Token[]): Result<RuleAssignBlock, string> {
  let lit = consumeLiteral(tokens, "let");
  if (!lit.ok) return lit;

  lit = consumeLiteral(tokens, "{");
  if (!lit.ok) return lit;

  const rule: RuleAssignBlock = { type: "assignBlock", assignments: [] };

  while (tokens[0].type !== "}") {
    const name = tokens.shift();
    if (name?.type !== "input") return err("expected input declaration");

    lit = consumeLiteral(tokens, "=");
    if (!lit.ok) return lit;

    const value = parseValue(tokens);
    if (!value.ok) return value;

    rule.assignments.push({
      type: "assignment",
      key: name.value,
      value: value.value,
    });
  }

  lit = consumeLiteral(tokens, "}");
  if (!lit.ok) return lit;

  lit = consumeLiteral(tokens, "in");
  if (!lit.ok) return lit;

  return ok(rule);
}

function parseObject(tokens: Token[]): Result<RuleObject, string> {
  let lit = consumeLiteral(tokens, "{");
  if (!lit.ok) return lit;

  const object: RuleObject = { type: "object", pairs: [] };

  let token = tokens[0];
  while (token && token.type !== "}") {
    switch (token.type) {
      case "..": {
        const spread = parseSpread(tokens);
        if (!spread.ok) return spread;
        object.pairs.push(spread.value);
        break;
      }
      case "pathSegment": {
        const pair = parsePair(tokens);
        if (!pair.ok) return pair;
        object.pairs.push(pair.value);
        break;
      }
      default:
        return err("expected `..` or path segment, got: " + token.type);
    }

    token = tokens[0];
  }

  lit = consumeLiteral(tokens, "}");
  if (!lit.ok) return lit;

  return ok(object);
}

function parsePair(tokens: Token[]): Result<RulePair, string> {
  if (tokens.length < 3) return err("not enough tokens");

  const path = parsePath(tokens);
  if (!path.ok) return path;

  const lit = consumeLiteral(tokens, "=");
  if (!lit.ok) return lit;

  const value = parseValue(tokens);
  if (!value.ok) return value;

  return ok({ type: "pair", path: path.value, value: value.value });
}

function parsePath(tokens: Token[]): Result<RulePath, string> {
  const rule: RulePath = { type: "path", value: [] };

  const token = tokens.shift();
  if (token?.type !== "pathSegment") return err("expected path segment");

  rule.value.push(token.value);

  while (tokens[0].type !== "=") {
    const lit = consumeLiteral(tokens, ".");
    if (!lit.ok) return lit;

    const token = tokens.shift();
    if (token?.type !== "pathSegment") return err("expected path segment");

    rule.value.push(token.value);
  }

  return ok(rule);
}

function parseValue(tokens: Token[]): Result<RuleValue, string> {
  const token = tokens[0];
  if (!token) return err("expected value");

  switch (token.type) {
    case "input":
    case "integer":
    case "float":
      tokens.shift();
      return ok(token);
    case "true":
      tokens.shift();
      return ok({ type: "boolean", value: true });
    case "false":
      tokens.shift();
      return ok({ type: "boolean", value: false });
    case "null":
      tokens.shift();
      return ok({ type: "null" });
    case '"':
      return parseString(tokens);
    case "{":
      return parseObject(tokens);
    case "[":
      return parseArray(tokens);
    default:
      return err("expected value token, got: " + token.type);
  }
}

function parseString(tokens: Token[]): Result<RuleString, string> {
  let lit = consumeLiteral(tokens, '"');
  if (!lit.ok) return lit;

  const rule: RuleString = { type: "string", value: [] };

  let token: Token = tokens[0];
  while (token && token.type !== '"') {
    switch (token.type) {
      case "charSequence":
      case "charEscape":
      case "unicodeEscape":
      case "input":
        tokens.shift();
        rule.value.push(token);
        break;
      default:
        return err(
          "expected one of {char sequence, char escape, unicode escape, input}, got: " +
            token.type,
        );
    }

    token = tokens[0];
  }

  lit = consumeLiteral(tokens, '"');
  if (!lit.ok) return lit;

  return ok(rule);
}

function parseArray(tokens: Token[]): Result<RuleArray, string> {
  let lit = consumeLiteral(tokens, "[");
  if (!lit.ok) return lit;

  const rule: RuleArray = { type: "array", values: [] };

  let token = tokens[0];
  while (token.type !== "]") {
    switch (token.type) {
      case "..": {
        const spread = parseSpread(tokens);
        if (!spread.ok) return spread;
        rule.values.push(spread.value);
        break;
      }
      default: {
        const value = parseValue(tokens);
        if (!value.ok) return value;
        rule.values.push(value.value);
        break;
      }
    }

    token = tokens[0];
  }

  lit = consumeLiteral(tokens, "]");
  if (!lit.ok) return lit;

  return ok(rule);
}

function parseSpread(tokens: Token[]): Result<RuleSpread, string> {
  const lit = consumeLiteral(tokens, "..");
  if (!lit.ok) return lit;

  const input = tokens.shift();
  if (input?.type !== "input") return err("expected input");

  return ok({ type: "spread", value: input.value });
}

/**
 * Consumes a literal token,
 * returning an error if is not present.
 *
 * @param tokens Token array
 * @param tokenType Token to consume
 */
function consumeLiteral(
  tokens: Token[],
  tokenType: TokenType,
): Result<void, string> {
  if (tokens.length === 0) return err("no tokens");

  const nextToken = tokens.shift();
  if (nextToken?.type !== tokenType) {
    return err(`expected '${tokenType}', got '${nextToken?.type}'`);
  }

  return ok(undefined);
}
