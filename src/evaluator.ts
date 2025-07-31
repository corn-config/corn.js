import {
  RuleArray,
  RuleAssignBlock,
  RuleConfig,
  RuleInput,
  RuleObject,
  RuleSpread,
  RuleString,
  RuleValue,
} from "./parser";
import { assertNever, err, isObject, ok, Result } from "./utils";
import dedent from "dedent";

export type Value =
  | boolean
  | number
  | string
  | null
  | Value[]
  | { [k: string]: Value };

export class Evaluator {
  private inputs: Record<string, Value> = {};

  public evaluate(rule: RuleConfig): Result<Record<string, Value>, string> {
    if (rule.assignBlock) {
      const res = this.evaluateAssignBlock(rule.assignBlock);
      if(!res.ok) return res;
    }

    const obj = this.evaluateObject(rule.object);
    if(!obj.ok) return obj;

    return ok(obj.value);
  }

  public clear() {
    this.inputs = {};
  }

  private evaluateAssignBlock(rule: RuleAssignBlock): Result<void, string> {
    for (const pair of rule.assignments) {
      const value = this.evaluateValue(pair.value);
      if(!value.ok) return value;
      this.inputs[pair.key] = value.value;
    }

    return ok(undefined);
  }

  private evaluateObject(rule: RuleObject): Result<Record<string, Value>, string> {
    const obj: Record<string, Value> = {};

    for (const pair of rule.pairs) {
      switch (pair.type) {
        case "pair":
          const val = this.evaluateValue(pair.value);
          if(!val.ok) return val;

          const res = this.addAtPath(obj, pair.path.value, val.value);
          if(!res.ok) return res;
          break;
        case "spread":
          const value = this.evaluateInput(pair);
          if(!value.ok) return value;

          if (!isObject(value.value))
            return err(`input is not an object: ${pair.value}`);

          for (const key in value.value as Record<string, Value>) {
            obj[key] = value.value[key as keyof Value];
          }
          break;
        default:
          assertNever(pair);
      }
    }

    return ok(obj);
  }

  private addAtPath(obj: Record<string, Value>, path: string[], value: Value): Result<void, string> {
    for (let i = 0; i < path.length; i++) {
      const key = path[i];

      if (i === path.length - 1) {
        obj[key] = value;
        break;
      }

      const existingObject = obj[key];
      if (existingObject === undefined) {
        obj[key] = {};
      } else if (!isObject(existingObject)) {
        return err(
          `expected object at path ${path.slice(0, i + 1).join(".")}`,
        );
      }

      obj = obj[key] as Record<string, Value>;
    }

    return ok(undefined);
  }

  private evaluateArray(rule: RuleArray): Result<Value[], string> {
    const array: Value[] = [];

    for (const value of rule.values) {
      switch (value.type) {
        case "spread":
          const inputVal = this.evaluateInput(value);
          if(!inputVal.ok) return inputVal;

          if (!Array.isArray(inputVal.value))
            return err(`input is not an array: ${value.value}`);

          array.push(...inputVal.value);
          break;
        default:
          const val = this.evaluateValue(value);
          if(!val.ok) return val;
          array.push(val.value);
      }
    }

    return ok(array);
  }

  private evaluateString(rule: RuleString): Result<string, string> {
    const result: string[] = [];

    for (const part of rule.value) {
      switch (part.type) {
        case "charSequence":
        case "charEscape":
          result.push(part.value);
          break;
        case "unicodeEscape":
          const MAX = 0x10FFFF;
          if (part.value > MAX) {
            return err('invalid code point');
          }
          result.push(String.fromCodePoint(part.value));
          break;
        case "input":
          const value = this.evaluateInput(part);
          if(!value.ok) return value;
          if (typeof value.value !== "string")
            return err(`input is not a string: ${part.value}`);

          result.push(...value.value);
          break;
        default:
          assertNever(part);
      }
    }

    let str = result.join("");

    if (str.includes("\n")) {
      str = this.trimMultilineString(str);
    }

    return ok(str);
  }

  private trimMultilineString(str: string): string {
    if (str.startsWith("\r")) {
      str = str.slice(1);
    }

    if (str.startsWith("\n")) {
      str = str.slice(1);
    }

    return dedent.withOptions({ trimWhitespace: false })(str);
  }

  private evaluateValue(rule: RuleValue): Result<Value, string> {
    switch (rule.type) {
      case "boolean":
      case "integer":
      case "float":
        return ok(rule.value);
      case "input":
        return this.evaluateInput(rule);
      case "string":
        return this.evaluateString(rule);
      case "array":
        return this.evaluateArray(rule);
      case "object":
        return this.evaluateObject(rule);
      case "null":
        return ok(null);
      default:
        assertNever(rule);
    }
  }

  private evaluateInput(rule: RuleInput | RuleSpread): Result<Value, string> {
    let value: Value | undefined;
    if (rule.value.startsWith("$env_")) {
      value = process.env[rule.value.slice("$env_".length)];
    }

    if (!value) {
      value = this.inputs[rule.value];
    }
    if (!value) return err(`input not found: ${rule.value}`);
    return ok(value);
  }
}

export class ObjectEvaluator extends Evaluator {

}

