import {SimpleDatum} from './simple_datum';

export class Number extends SimpleDatum<number> {
  constructor(x: number) {
    super(x);
  }
}