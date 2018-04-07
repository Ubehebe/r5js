import {SchemeSources} from "./scheme_sources";
import {AsyncEvaluator} from "../eval/async_evaluator";
import {NULL_INPUT_PORT} from "../io/input_port";
import {CallbackBackedPort} from "../io/callback_backed_port";
import {ResultStruct, stringToResultStruct, TestFramework} from "./test_framework";

const sources = SchemeSources.get();
const evaluator = new AsyncEvaluator(NULL_INPUT_PORT, new CallbackBackedPort(handleWriteFromScheme));
let numFailures: number;

/**
 * Runs the unit tests written in Scheme (//scm/r5rs-tests.scm, etc.).
 *
 * The Scheme unit tests do not correctly propagate their counts (number of tests
 * run/failed/succeeded) to jsunit. However, they do correctly propagate whether they succeeded
 * or failed, and jsunit propagates this to bazel, so they are an effective pre-commit hook.
 *
 * TODO: write a Skylark scheme_test macro or rule that hides all the JS implementation details.
 * It could just invoke node and have the Scheme tests write a proto or JSON.
 * But we need better node support before doing that.
 */
describe("pure scheme tests", () => {
  beforeEach(() => {
    numFailures = 0;
  });

  it("should evaluate the test framework without problems", (done) => {
    new TestFramework(sources).execute().then(result => {
      expect(result).not.toBeNull();
      done();
    });
  });

  it("should run r5rs-tests.scm successfully", (done) => {
    evaluator.evaluate(sources.testFramework + sources.r5RSTests).then(result => {
      expect(result).not.toBeNull();
      expect(numFailures).toBe(0);
      done();
    });
  });

  it("should run negative-tests.scm successfully", (done) => {
    evaluator.evaluate(sources.testFramework + sources.negativeTests).then(result => {
      expect(result).not.toBeNull();
      expect(numFailures).toBe(0);
      done();
    });
  });

  it("should run other-tests.scm successfully", (done) => {
    evaluator.evaluate(sources.testFramework + sources.otherTests).then(result => {
      expect(result).not.toBeNull();
      expect(numFailures).toBe(0);
      done();
    });
  });
});

function handleWriteFromScheme(str: string) {
  let result: ResultStruct | string | null = stringToResultStruct(str);
  if (result) {
    const success = result.numFailed === 0;
    if (success) {
      console.log(`[ok] r5js.test.SchemeTest ${result.name}`);
    } else {
      console.error(`[fail] r5js.test.SchemeTest ${result.name}`);
    }
  } else if (result = stringToFailureMessage(str)) {
    ++numFailures;
    console.error(`[fail] r5js.test.SchemeTest ${result}`);
  }
}

/**
 * Parses a Scheme test framework output like `(fail foo-tests (input (+ 1 1)) (want 3) (got 2))`
 * into a string, returning null if the parse failed.
 */
function stringToFailureMessage(str: string): string | null {
  const match = /\(fail .+ \(input (.*)\) \(want (.*)\) \(got (.*)\)\)/.exec(str);
  return match && `input ${match[1]}: want ${match[2]}, got ${match[3]}`;
}