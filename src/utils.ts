/**
 * Represents the outcome of a fallible operation,
 * where the outcome can be either success value `T` or error `E`.
 */
export type Result<T, E> = { ok: true; value: T } | { ok: false; error: E };

/**
 * Creates an ok (success) variant of `Result`.
 * @param value
 */
export function ok<T, E>(value: T): Result<T, E> {
  return { ok: true, value };
}

/**
 * Creates an error variant of `Result`.
 * @param error
 */
export function err<T, E>(error: E): Result<T, E> {
  return { ok: false, error };
}

/**
 * Throws an "unreachable" error if it ever runs.
 *
 * This is used as an exhaustive typeguard check on switch blocks,
 * causing the type checker to error if a non-never type is passed in.
 * @param _
 */
// eslint-disable-next-line @typescript-eslint/no-unused-vars
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

export function isObject(
  val: unknown,
): val is Record<string | number | symbol, unknown> {
  return typeof val === "object" && val !== null && !Array.isArray(val);
}
