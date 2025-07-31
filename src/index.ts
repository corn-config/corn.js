import { Lexer, LexerError } from "./lexer";
import { parseTokens } from "./parser";
import { Evaluator, Value } from "./evaluator";
import { err, Result } from "./utils";

export type { LexerError } from './lexer';
export type { Value } from './evaluator';
export type { Result } from './utils'

export function parse(
  corn: string,
): Result<Record<string, Value>, string | LexerError> {
  const lexer = new Lexer();
  const tokensRes = lexer.tokenizeInput(corn);

  if (!tokensRes.ok) return err(tokensRes.error);

  const tokens = tokensRes.value;
  const astRes = parseTokens(tokens.map((t) => t.token));

  if (!astRes.ok) return err(astRes.error);

  const ast = astRes.value;
  const evaluator = new Evaluator();
  return evaluator.evaluate(ast);
}

// const input = fs
//   .readFileSync("test-suite/corn/array/single.pos.corn", "utf-8");
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
