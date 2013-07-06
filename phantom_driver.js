/* This file lives outside the Closure build system; accordingly, it is
 outside the src/ directory. Its purpose is to run the application's unit tests
 (which do live inside the Closure build system) with PhantomJS, a headless
 WebKit. The command-line invocation looks like this:

 $ phantomjs phantom_driver.js [URL of test HTML file]

 This will run the below script, which loads the actual test HTML page
 and evaluates its r5js.test.main function in a blocking manner.
 r5js.test.main returns an error code appropriate for returning
 to the command line.

 It would be nice if we could load the test HTML directly from the command
 line, but PhantomJS would still need to know when to exit. That is the main
 function of this driver. */

var system = require('system');
var testUrl = system.args[1];
var page = require('webpage').create();

// Pass console.log calls in the page through to stdout.
page.onConsoleMessage = function(message) {
    console.log(message);
    var resultStruct = parseResultStruct(message);
    if (resultStruct) {
        // The number of failed tests is not a useful Unix error code.
        phantom.exit(resultStruct.numFailed > 0 ? 1 : 0);
    }
};

page.open(testUrl, function(status) {
    if (status !== 'success') {
        console.log('Unable to open URL for testing: ' + testUrl);
        phantom.exit(1);
    }
    page.evaluate(function() {
        r5js.test.main();
    });
});

function parseResultStruct(string) {
    var re = /^end of testing: (\d+) tests, (\d+) failures$/;
    var ans = re.exec(string);
    if (ans && ans[0] && ans[1] && ans[2]) {
        var num = Number(ans[2]);
        return {numFailed: num};
    } else {
        return null;
    }
}