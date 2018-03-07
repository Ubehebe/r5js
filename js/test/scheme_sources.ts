import {NEGATIVE_TESTS} from '../../scm/negative-tests';
import {OTHER_TESTS} from '../../scm/other-tests';
import {R5RS_TESTS} from '../../scm/r5rs-tests';
import {TEST_FRAMEWORK} from '../../scm/unit-test';
import {TEST_FRAMEWORK_TESTS} from '../../scm/unit-test-tests';

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
