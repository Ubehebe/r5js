package r5js;

import com.google.common.collect.ImmutableList;

enum Platform {
    ANDROID("android", ImmutableList.<String>of()),
    HTML5("html5", ImmutableList.<String>of()),
    NODE("node", ImmutableList.<String>of());

    final String closureDefineName;
    final ImmutableList<String> closureEntryPoints;

    Platform(String closureDefineName, ImmutableList<String> closureEntryPoints) {
        this.closureDefineName = closureDefineName;
        this.closureEntryPoints = closureEntryPoints;
    }
}
