goog.provide('r5js.js.NodeEnvironment');


goog.require('goog.Promise');
goog.require('r5js.NodeBackedPort');



/**
 * @implements {r5js.js.Environment}
 * @struct
 * @constructor
 */
r5js.js.NodeEnvironment = function() {};


/** @override */
r5js.js.NodeEnvironment.prototype.fetchUrl = function(url) {
  return new goog.Promise(function(resolve, reject) {
    var fs = require('fs');
    fs.readFile('.' + url, function(err, data) {
      if (err) {
        reject(err);
      } else {
        resolve(data.toString());
      }
    });
  });
};


/** @override */
r5js.js.NodeEnvironment.prototype.exit = function(statusCode) {
  process.exit(statusCode);
};


/** @override */
r5js.js.NodeEnvironment.prototype.newInputPort = function(name) {
  return new r5js.NodeBackedPort(name, 'r');
};


/** @override */
r5js.js.NodeEnvironment.prototype.newOutputPort = function(name) {
  return new r5js.NodeBackedPort(name, 'w');
};
