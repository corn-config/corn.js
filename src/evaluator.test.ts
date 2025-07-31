import fs from "fs";
import { glob } from "glob";
import { Evaluator } from "./evaluator";
import { Lexer, LexerError } from "./lexer";
import { parseTokens } from "./parser";

const positiveCases = glob
  .sync("test-suite/corn/**/*.pos.corn")
  .map((p) => p.replaceAll("\\", "/"));

const negativeCases = glob
  .sync("test-suite/corn/**/*.neg.corn")
  .map((p) => p.replaceAll("\\", "/"));

type TestGroup = {
  [key: string]: TestGroup | string;
};

function makeTree(cases: string[]): TestGroup {
  const testGroups: TestGroup = {};
  cases.forEach(function (path) {
    path
      .replace("test-suite/corn/", "")
      .split("/")
      .reduce((group, section, i, arr) => {
        if (i === arr.length - 1) {
          group[section.replace(".pos.corn", "").replace(".neg.corn", "")] =
            path;
          return group;
        }

        if (!group[section]) {
          group[section] = {};
        }

        return group[section] as TestGroup;
      }, testGroups);
  });

  return testGroups;
}

function positiveTest(key: string, value: string) {
  const jsonPath = value
    .replace("/corn/", "/json/")
    .replace(".pos.corn", ".json");

  const corn = fs.readFileSync(value, "utf-8").replaceAll("\r\n", "\n"); // CRLF --> LF, thanks Windows

  const jsonFile = fs.readFileSync(jsonPath, "utf-8");
  const json = JSON.parse(jsonFile);

  test(key, () => {
    const tokens = new Lexer().tokenizeInput(corn);

    expect(tokens.ok).toBe(true);
    if (!tokens.ok) return; // make typescript happy

    const ast = parseTokens(tokens.value.map((t) => t.token));

    expect(ast.ok).toBe(true);
    if (!ast.ok) return;

    const evaluator = new Evaluator();
    const result = evaluator.evaluate(ast.value);

    expect(result.ok).toBe(true);
    if (!result.ok) return;

    expect(result.value).toEqual(json);
  });
}

function negativeTest(key: string, value: string) {
  const corn = fs.readFileSync(value, "utf-8").replaceAll("\r\n", "\n"); // CRLF --> LF, thanks Windows

  test(key, () => {
    let hasErr = false;
    let err: string | LexerError | undefined;

    const tokens = new Lexer().tokenizeInput(corn);

    if (!tokens.ok) {
      hasErr = true;
      err = tokens.error;
    } else {
      const ast = parseTokens(tokens.value.map((t) => t.token));
      if (!ast.ok) {
        hasErr = true;
        err = ast.error;
      } else {
        const evaluator = new Evaluator();
        const result = evaluator.evaluate(ast.value);
        if (!result.ok) {
          hasErr = true;
          err = result.error;
        }
      }
    }

    console.log(err);

    expect(hasErr).toBe(true);
  });
}

function evaluate(
  group: TestGroup,
  tester: (key: string, value: string) => void,
) {
  for (const key in group) {
    const value = group[key];
    if (typeof value === "object") {
      describe(key, () => {
        evaluate(value as TestGroup, tester);
      });
    } else {
      tester(key, value);
    }
  }
}

describe("positive", () => evaluate(makeTree(positiveCases), positiveTest));
describe("negative", () => evaluate(makeTree(negativeCases), negativeTest));
