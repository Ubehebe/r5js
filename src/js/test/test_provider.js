goog.provide('r5js.test.Provider');


goog.require('r5js.test.Scanner');


/**
 * @implements {tdd.TestSuiteProvider}
 * @constructor
 */
r5js.test.Provider = function() {};


/** @override */
r5js.test.Provider.prototype.getTestSuites = function() {
    return [
        new r5js.test.Scanner()
    ];
};
