import {PROCEDURES} from './scm/r5rs-procedures';
import {SYNTAX} from './scm/r5rs-syntax';

export class SchemeSources {
  readonly syntax: string = SYNTAX;
  readonly procedures: string = PROCEDURES;
}
