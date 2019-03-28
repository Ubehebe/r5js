import {AsyncEvaluator} from "../eval/async_evaluator";
import {CallbackBackedPort} from "../io/callback_backed_port";
import {NULL_INPUT_PORT} from "../io/input_port";
import {SchemeSources} from "../scm/test_sources";

export class TestFramework {

  private actualResult = new ResultStruct();

  constructor(private readonly sources: SchemeSources) {}

  private onWrite_(str: string) {
    const result = stringToResultStruct(str);
    if (result) {
      console.log(`[ok] r5js.test.TestFramework ${result.name}`);
      this.actualResult = this.actualResult.merge(result);
    }
  }

  execute(): Promise<ResultStruct> {
    const evaluator = new AsyncEvaluator(
        NULL_INPUT_PORT, new CallbackBackedPort(output => this.onWrite_(output)));
    return evaluator.evaluate(this.sources.testFramework + this.sources.testFrameworkTests)
        .then(() => resultIsExpected(this.actualResult))
        .then(success => success ? new ResultStruct(1, 0) : new ResultStruct(0, 1))
        .then(result => Promise.resolve(result));
  }
}

/** Must be kept manually in sync with the expected results of test/unit-test-tests.scm. */
function resultIsExpected(result: ResultStruct): boolean {
  return result.numRun() === 3
      && result.numSucceeded === 2
      && result.numFailed === 1;
}

/**
 * Parses a Scheme test framework output like this: `(foo-tests (3 tests) (1 failed))` into a
 * ResultStruct, returning null if the parse failed.
 */
export function stringToResultStruct(str: string): ResultStruct | null {
  const regex = /\((.+) \((\d+) tests\) \((\d+) failed\)\)/;
  const matches = regex.exec(str);
  if (!matches) {
    return null;
  }
  const name = matches[1];
  const numSucceeded = parseInt(matches[2], 10);
  const numFailed = parseInt(matches[3], 10);
  return new ResultStruct(numSucceeded, numFailed, name);
}

export class ResultStruct {
  constructor(
      readonly numSucceeded: number = 0,
      readonly numFailed: number = 0,
      readonly name?: string) {}

  numRun(): number {
    return this.numSucceeded + this.numFailed;
  }

  merge(other: ResultStruct): ResultStruct {
    return new ResultStruct(
        this.numSucceeded + other.numSucceeded,
        this.numFailed + other.numFailed);
  }
}
