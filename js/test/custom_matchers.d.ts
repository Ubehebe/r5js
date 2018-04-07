declare namespace jasmine {
  interface Matchers<T> {
    toEvalTo(expected: any): boolean;
    toOutput(expected: any): boolean;
    toParseAs(expected: any): boolean;
    // TODO: should be new (any) => Datum. Why doesn't that compile?
    toScanAs(expected: new (arg: any) => any): boolean;
    toThrow(expected: any): boolean;
  }
}