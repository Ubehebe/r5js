declare namespace jasmine {
  interface Matchers<T> {
    toParseAs(expected: any): boolean;
    // TODO: should be new (any) => Datum. Why doesn't that compile?
    toScanAs(expected: new (any) => any): boolean;
  }
}