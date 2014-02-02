goog.provide('r5js.test.Provider');


goog.require('r5js.test.Scanner');


/**
 * @struct
 * @constructor
 */
r5js.test.Provider = function() {};


/** @return {!Array.<!tdd.TestSuite>} */
r5js.test.Provider.prototype.getTestSuites = function() {
    return [
        new r5js.test.Scanner()
    ];
};
