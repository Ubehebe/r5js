package r5js;

enum EntryPoint {

    ANDROID_MAIN("r5js.platform.android.main"),
    HTML5_WORKER("r5js.platform.html5.Worker"),
    HTML5_REPL_MAIN("r5js.platform.html5.repl"),
    NODE_REPL_MAIN("r5js.platform.node.repl"),
    TEST_MAIN("r5js.test.main");

    private final String jsSymbol;

    EntryPoint(String jsSymbol) {
        this.jsSymbol = jsSymbol;
    }

    String getEntryPoint() {
        return jsSymbol;
    }
}
