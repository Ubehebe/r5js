package r5js;

enum EntryPoint {

    HTML5_WORKER("r5js.platform.html5.Worker"),
    TEST_MAIN("r5js.test.main");

    private final String jsSymbol;

    EntryPoint(String jsSymbol) {
        this.jsSymbol = jsSymbol;
    }

    public String getEntryPoint() {
        return jsSymbol;
    }
}
