


CLOSURE_BASE_PATH = '../../../closure-library/closure/goog/';
importScripts(
    CLOSURE_BASE_PATH + 'bootstrap/webworkers.js',
    CLOSURE_BASE_PATH + 'base.js',
    CLOSURE_BASE_PATH + 'deps.js',
    '../../../build/deps.js');

window = this.window || {clearTimeout: function() {}};


goog.require('r5js.test.main');
goog.require('r5js.test.evalSandbox');
goog.require('r5js.test.parseSandbox');
goog.require('r5js.test.readSandbox');


addEventListener('message', function(e) {
  var arg = e.data.args && e.data.args[0];
  switch (e.data.name) {
    case 'r5js.test.main':
      r5js.test.main();
      break;
    case 'r5js.test.readSandbox':
      r5js.test.readSandbox(arg);
      break;
    case 'r5js.test.parseSandbox':
      r5js.test.parseSandbox(arg);
      break;
    case 'r5js.test.evalSandbox':
      r5js.test.evalSandbox(arg);
      break;
    default:
      break;
  }
});
