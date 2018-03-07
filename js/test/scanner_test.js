goog.module('r5js.test.Scanner');

const {Boolean} = require('/js/eval/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/boolean');
const {Character} = require('/js/eval/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/character');
const {Identifier} = require('/js/eval/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/identifier');
const {Number} = require('/js/eval/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/number');
const {String} = require('/js/eval/shim_collect_es6_sources.es6/node_modules/__main__/js/ast/string');
const expect = goog.require('expect');
const scanAs = goog.require('scanAs');
const testSuite = goog.require('goog.testing.testSuite');
goog.require('goog.testing.jsunit');

testSuite({

  testBooleans() {
    ['#t', '#f', '#T', '#F'].forEach(text => expect(text).to(scanAs(Boolean)));
    ['##f', '#', '#'].forEach(text => expect(text).not().to(scanAs(Boolean)));
  },

  testCharacters() {
    ['#\\c', '#\\space', '#\\newline', '#\\\\'].forEach(text => expect(text).to(scanAs(Character)));
  },

  testIdentifiers() {
    ['h', '+', '-', '...', '!', '$', '%', '&', '*', '/', ':', '<', '=', '>',
      '?', '~', '_', '^', '&+', 'h+...@@@-.'].forEach(text => expect(text).to(scanAs(Identifier)));
    ['|', '[', ']', '{', '}'].forEach(text => expect(text).not().to(scanAs(Identifier)));
  },

  testNumbers() {
    getValidNumberTokens_().forEach(text => expect(text).to(scanAs(Number)));
    ['1+2'].forEach(text => expect(text).not().to(scanAs(Number)));
  },

  testStrings() {
    ['""', '"hello, world"', '" \\" "', '"\\\\"'].forEach(text => expect(text).to(scanAs(String)));
    ['"', '\\'].forEach(text => expect(text).not().to(scanAs(String)));
  }
});

/**
 * @return {!Array<string>}
 * @private
 */
function getValidNumberTokens_() {
  /* TODO bl: adding in these prefixes creates a huge number of test cases
     (more than 30,000) with about half of them failing.
    let prefixes = r5js.test.Scanner.getValidNumberPrefixes_(); */
  const suffixes = getValidNumberSuffixes_();
  const validDecimals = ['8762', '-3', '4987566###', '.765', '.549867#', '0.',
    '37.###', '565.54', '3765.4499##', '4##.', '56#.', '587##.#'];
  const validNumberTokens = [];
  for (let i = 0; i < validDecimals.length; ++i) {
    for (let j = 0; j < suffixes.length; ++j) {
      validNumberTokens.push(validDecimals[i] + suffixes[j]);
    }
  }
  return validNumberTokens;
}

/**
 * @return {!Array<string>}
 * @private
 */
function getValidNumberPrefixes_() {
  const bases = ['', '#b', '#B', '#o', '#O', '#d', '#D', '#x', '#X'];
  const exactnesses = ['', '#e', '#E', '#i', '#I'];
  const prefixes = [];
  let i, j;
  for (i = 0; i < bases.length; ++i) {
    for (j = 0; j < exactnesses.length; ++j) {
      prefixes.push(bases[i] + exactnesses[j]);
      prefixes.push(exactnesses[j] + bases[i]);
    }
  }
  return prefixes;
}

/**
 * @return {!Array<string>}
 * @private
 */
function getValidNumberSuffixes_() {
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
