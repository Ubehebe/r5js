import {
  NEGATIVE_TESTS,
  OTHER_TESTS,
  R5RS_TESTS,
  UNIT_TEST as TEST_FRAMEWORK,
  UNIT_TEST_TESTS as TEST_FRAMEWORK_TESTS
} from './tests';

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
