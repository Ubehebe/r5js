class Nil implements ObjectValue {
  eqv(other: Value): boolean {
    return this === other;
  }
}

export const NIL: Value = new Nil();