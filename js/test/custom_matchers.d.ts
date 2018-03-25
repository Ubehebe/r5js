declare namespace jasmine {
  interface Matchers<T> {
    // TODO: should be new (any) => Datum. Why doesn't that compile?
    toScanAs(expected: new (any) => any): boolean;
  }
}