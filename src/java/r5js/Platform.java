package r5js;

import com.google.common.collect.ImmutableList;

/**
 * A platform represents the JavaScript environment in which a specific {@link Target}
 * runs.
 *
 * <p>Platforms have platform-specific capabilities, which are made known to the compiler
 * through extern files.
 */
enum Platform {
    ANDROID("custom-externs/android.js"),
    HTML5,
    NASHORN,
    NODE("externs/process.js");

    private final ImmutableList<String> externs;


    Platform(String... externs) {
        this.externs = ImmutableList.copyOf(externs);
    }

    @Override
    public String toString() {
        return super.toString().toLowerCase();
    }

    ImmutableList<String> externs() {
        return externs;
    }
}
