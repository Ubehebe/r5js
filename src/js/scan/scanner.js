goog.provide('r5js.Scanner');



/** @interface */
r5js.Scanner = function() {};


/** @return {r5js.Token} The next token, or null if there are no more. */
r5js.Scanner.prototype.nextToken = function() {};
