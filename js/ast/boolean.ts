import {SimpleDatum} from './simple_datum';

export class Boolean extends SimpleDatum<boolean> {
  constructor(val: boolean) {
    super(val);
  }
}
