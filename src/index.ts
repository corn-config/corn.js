import { Lexer, LexerError } from "./lexer";
import { parseTokens } from "./parser";
import { Evaluator, Value } from "./evaluator";
import { err, Result } from "./utils";

export type { LexerError } from "./lexer";
export type { Value } from "./evaluator";
export type { Result } from "./utils";

/**
 * Parses the provided corn string
 * into an object.
 *
 * @typeParam T output object type
 * @param corn input string
 * @returns result containing output object on success,
 * or error variant on any failure.
 */
export function parse<T extends Record<string, Value> = Record<string, Value>>(
  corn: string,
): Result<T, string | LexerError> {
  // strip bom
  if (corn.charCodeAt(0) === 0xfeff) {
    corn = corn.slice(1);
  }

  const lexer = new Lexer();
  const tokensRes = lexer.tokenizeInput(corn);

  if (!tokensRes.ok) return err(tokensRes.error);

  const tokens = tokensRes.value;
  const astRes = parseTokens(tokens.map((t) => t.token));

  if (!astRes.ok) return err(astRes.error);

  const ast = astRes.value;
  const evaluator = new Evaluator();
  return evaluator.evaluate(ast) as Result<T, string | LexerError>;
}

/* ---- */

// import fs from "fs";
// const input = fs
//   .readFileSync("test-suite/corn/input/basic.pos.corn", "utf-8");
//
// console.log(input);
//
// const tokens = new Lexer().tokenizeInput(input);
//
// if(!tokens.ok) {
//   console.error(tokens.error);
//   process.exit(1);
// }
//
// console.log('TOKENS', tokens.value);
//
// console.log("\n---------------\n");
//
// const ast = parseTokens(tokens.value.map(t => t.token));
//
// if(!ast.ok) {
//   console.error(ast.error);
//   process.exit(1);
// }
//
// console.log("\nAST:");
// console.dir(ast.value, { depth: null });
//
// console.log("\n---------------\n");
//
// const obj = new Evaluator().evaluate(ast.value);
//
// if(!obj.ok) {
//   console.error(obj.error);
//   process.exit(1);
// }
//
// console.log("\nOBJ:");
// console.dir(obj.value, { depth: null });
