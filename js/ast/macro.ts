import {SimpleDatum} from './simple_datum';

export class Macro extends SimpleDatum<any /* TODO should be r5js.Macro */> {
  constructor(macro: any) {
    super(macro);
  }

  getMacro(): any {
    return this.payload.setIsLetOrLetrecSyntax();
  }
}