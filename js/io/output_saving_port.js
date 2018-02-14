goog.module('r5js.OutputSavingPort');

const {OutputPort} = require('/js/io/output_port_collect_es6_sources.es6/node_modules/__main__/js/io/output_port');

class OutputSavingPort extends OutputPort {
 constructor() {
   super();
 }

 /** @return {?string} */
 dequeueOutput() {}
}

exports = OutputSavingPort;


