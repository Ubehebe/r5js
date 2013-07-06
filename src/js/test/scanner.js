goog.provide('r5js.test.Scanner');


goog.require('tdd.SyncTestSuite');


/**
 * @extends {tdd.SyncTestSuite}
 * @constructor
 */
r5js.test.Scanner = function() {
    goog.base(this, "r5js.test.Scanner");
};
goog.inherits(r5js.test.Scanner, tdd.SyncTestSuite);



/** @override */
r5js.test.Scanner.prototype.runTests = function() {
    var sum;
    this.Do('one plus one',
        function() {
            sum = 1+1;
        }).expecting(sum).toBe(2);
};