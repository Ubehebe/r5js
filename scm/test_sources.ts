import {NEGATIVE_TESTS} from './NEGATIVE_TESTS';
import {OTHER_TESTS} from './OTHER_TESTS';
import {R5RS_TESTS} from './R5RS_TESTS';
import {TEST_FRAMEWORK} from './TEST_FRAMEWORK';
import {TEST_FRAMEWORK_TESTS} from './TEST_FRAMEWORK_TESTS';

export class SchemeSources {

  readonly testFramework: string = TEST_FRAMEWORK;
  readonly testFrameworkTests: string = TEST_FRAMEWORK_TESTS;
  readonly r5RSTests: string = R5RS_TESTS;
  readonly negativeTests: string = NEGATIVE_TESTS;
  readonly otherTests: string = OTHER_TESTS;

  static get(): SchemeSources {
    return new SchemeSources();
  }
}
