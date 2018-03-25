import {Boolean} from "../ast/boolean";
import {Character} from "../ast/character";
import {Identifier} from "../ast/identifier";
import {Number} from "../ast/number";
import {String} from "../ast/string";
import {TokenStream} from "../scan/token_stream";

describe("scanner", () => {
  beforeEach(() => {
    jasmine.addMatchers({toScanAs});
  });

  describe("booleans", () => {
    it("should accept well-formed booleans", () => {
      ['#t', '#f', '#T', '#F'].forEach(text => expect(text).toScanAs(Boolean));
    });

    it("should reject malformed booleans", () => {
      ['##f', '#', '#'].forEach(text => expect(text).not.toScanAs(Boolean));
    });
  });

  describe("characters", () => {
    it("should accept well-formed characters", () => {
      ['#\\c', '#\\space', '#\\newline', '#\\\\'].forEach(text => expect(text).toScanAs(Character));
    });
  });

  describe("identifiers", () => {
    it("should accept well-formed identifiers", () => {
      ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>',
        '?', '~', '_', '^', '&+', 'h+...@@@-.'].forEach(text => expect(text).toScanAs(Identifier));
    });

    it("should reject malformed identifiers", () => {
      ['|', '[', ']', '{', '}'].forEach(text => expect(text).not.toScanAs(Identifier));
    });
  });

  describe("numbers", () => {
    it("should accept well-formed numbers", () => {
      getValidNumberTokens().forEach(text => expect(text).toScanAs(Number));
    });

    it("should reject malformed numbers", () => {
      expect("1+2").not.toScanAs(Number);
    })
  });

  describe("strings", () => {
    it("should accept well-formed strings", () => {
      ['""', '"hello, world"', '" \\" "', '"\\\\"'].forEach(text => expect(text).toScanAs(String));
    });

    it("should reject malformed strings", () => {
      ['"', '\\'].forEach(text => expect(text).not.toScanAs(String));
    });
  });
});

function toScanAs(util: jasmine.MatchersUtil, customEqualityTesters: jasmine.CustomEqualityTester[]): jasmine.CustomMatcher {
  return {
    compare(actual: any, expected: any): jasmine.CustomMatcherResult {
      try {
        const scanner = TokenStream.forText(actual);
        const token = scanner.nextToken();
        // There should be exactly one token in the input. (For example, 1+2 should fail to scan as
        // one number token, even though the whole input scans.)
        if (!token || scanner.nextToken()) {
          return {
            pass: false,
          };
        }
        const pass = token instanceof expected;
        const message = pass
            ? undefined
            : `expected ${actual} to scan as ${expected}`;
        return {pass, message};
      } catch (e) {
        // some tests purposely cause scan errors
        return {
          pass: false,
          message: e.message,
        };
      }
    }
  }
}

function getValidNumberTokens(): string[] {
  // TODO bl: adding in these prefixes creates a huge number of test cases (more than 30,000) with
  // about half of them failing.
  // const prefixes = getValidNumberPrefixes();
  const suffixes = getValidNumberSuffixes();
  const validDecimals = ['8762', '-3', '4987566###', '.765', '.549867#', '0.',
    '37.###', '565.54', '3765.4499##', '4##.', '56#.', '587##.#'];
  const validNumberTokens: string[] = [];
  for (let i = 0; i < validDecimals.length; ++i) {
    for (let j = 0; j < suffixes.length; ++j) {
      validNumberTokens.push(validDecimals[i] + suffixes[j]);
    }
  }
  return validNumberTokens;
}

function getValidNumberPrefixes(): string[] {
  const bases = ['', '#b', '#B', '#o', '#O', '#d', '#D', '#x', '#X'];
  const exactnesses = ['', '#e', '#E', '#i', '#I'];
  const prefixes: string[] = [];
  let i, j;
  for (i = 0; i < bases.length; ++i) {
    for (j = 0; j < exactnesses.length; ++j) {
      prefixes.push(bases[i] + exactnesses[j]);
      prefixes.push(exactnesses[j] + bases[i]);
    }
  }
  return prefixes;
}

function getValidNumberSuffixes(): string[] {
  const exponentMarkers = ['e', 's', 'f', 'd', 'l', 'E', 'S', 'F', 'D', 'L'];
  const signs = ['', '+', '-'];
  const suffixes = [''];

  for (let i = 0; i < exponentMarkers.length; ++i) {
    for (let j = 0; j < signs.length; ++j) {
      suffixes.push(exponentMarkers[i] + signs[j] + '2387');
    }
  }
  return suffixes;
}

