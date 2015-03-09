package r5js;

enum EntryPoint {

    ANDROID_MAIN("r5js.platform.android.main"),
    HTML5_WORKER("r5js.platform.html5.Worker"),
    REPL_MAIN("r5js.repl.main"),
    TEST_MAIN("r5js.test.main");

    private final String jsSymbol;

    EntryPoint(String jsSymbol) {
        this.jsSymbol = jsSymbol;
    }

    public String getEntryPoint() {
        return jsSymbol;
    }
}
