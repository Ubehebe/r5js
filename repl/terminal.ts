/**
 * "Dumb" terminal that {@link r5js.Repl} can read to and write from.
 * In contrast to a Repl, a Terminal knows nothing about Scheme.
 */
export interface Terminal {
 getNextLineOfInput(): Promise<string>;

 print(str: string): void;

 error(str: string): void;
}
