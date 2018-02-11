export class Type {
    constructor(private readonly name: string) {}
    getName(): string {
        return this.name;
    }
}

/**
 * This is basically the enumeration of types in R5RS 3.2.
 * Ordinarily, type tags are a code smell in object-oriented code.
 * But this implementation is not completely object-oriented; in particular,
 * Scheme booleans, numbers, and symbols are represented by
 * corresponding JavaScript primitives, not objects.
 * It is convenient to have names for these types, for example
 * when printing errors during runtime type-checking. But for the most part
 * they have no critical role in this implementation.
 */
export const Types: {[name: string]: Type} /* TODO: learn more about ts index types */ = {
  BOOLEAN: new Type('boolean'),
  CHARACTER: new Type('char'),
  ENVIRONMENT_SPECIFIER: new Type('environment-specifier'),
  INPUT_PORT: new Type('input-port'),
  NUMBER: new Type('number'),
  OUTPUT_PORT: new Type('output-port'),
  PAIR: new Type('pair'),
  PROCEDURE: new Type('procedure'),
  STRING: new Type('string'),
  SYMBOL: new Type('symbol'),
  VECTOR: new Type('vector')
};

