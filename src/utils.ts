export type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };

export function ok<T, E>(value: T): Result<T, E> {
  return { ok: true, value };
}

export function err<T, E>(error: E): Result<T, E> {
  return { ok: false, error };
}

export function assertNever(_: never): never {
  throw new Error("unreachable");
}

/**
 * Determines if the given character is a numeric digit (0-9).
 *
 * @param {string} char - A single character to evaluate.
 * @return {boolean} True if the character is a digit, otherwise false.
 */
export function isDigit(char: string): boolean {
  return char.charCodeAt(0) >= 48 && char.charCodeAt(0) <= 57;
}

/**
 * Determines if the given character is a hex digit (0-F), case-insensitive.
 *
 * @param {string} char - A single character to evaluate.
 * @return {boolean} True if the character is a digit, otherwise false.
 */
export function isHexDigit(char: string): boolean {
  return (
    isDigit(char) ||
    (char >= "a" && char <= "f") ||
    (char >= "A" && char <= "F")
  );
}

/**
 * Determines if a given character is a letter (uppercase or lowercase) `[a-zA-Z]`.
 *
 * @param {string} char - The character to be checked.
 * @return {boolean} Returns true if the character is a letter, otherwise false.
 */
export function isLetter(char: string): boolean {
  return (
    (char.charCodeAt(0) >= 65 && char.charCodeAt(0) <= 90) ||
    (char.charCodeAt(0) >= 97 && char.charCodeAt(0) <= 122)
  );
}

export function isAlphanumeric(char: string): boolean {
  return isDigit(char) || isLetter(char);
}

export function isObject(val: unknown): val is Record<any, any> {
  return typeof val === "object" && val !== null && !Array.isArray(val);
}
