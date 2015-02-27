package r5js;

import com.google.common.collect.ImmutableList;

enum Platform {
    ANDROID("android", ImmutableList.of("r5js.test.main")),
    HTML5("html5", ImmutableList.of("r5js.test.main")),
    NODE("node", ImmutableList.of("r5js.test.main"));

    final String closureDefineName;
    final ImmutableList<String> closureEntryPoints;

    Platform(String closureDefineName, ImmutableList<String> closureEntryPoints) {
        this.closureDefineName = closureDefineName;
        this.closureEntryPoints = closureEntryPoints;
    }
}
