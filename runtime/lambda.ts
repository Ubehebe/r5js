import {SimpleDatum} from "../ast/simple_datum";
import {Procedure} from "./procedure";

export class Lambda extends SimpleDatum<Procedure> {
  constructor(private readonly name: string, procedure: Procedure) {
    super(procedure);
  }

  /** @return {string} */
  getName(): string {
    return this.name;
  }
}
