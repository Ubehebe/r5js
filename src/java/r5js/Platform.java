package r5js;

import com.google.common.collect.ImmutableList;

/**
 * A platform represents the JavaScript environment in which a specific {@link r5js.CompilationUnit}
 * runs.
 *
 * <p>Platforms have platform-specific capabilities, which are made known to the compiler
 * through extern files.
 */
enum Platform {

    ANDROID("custom-externs/android.js"),
    EMBEDDED,
    HTML5,
    NASHORN,
    NODE(
            "externs/core.js",
            "externs/events.js",
            "externs/process.js",
            "externs/readline.js");

    private final ImmutableList<String> externs;

    Platform(String... externs) {
        this.externs = ImmutableList.copyOf(externs);
    }

    public ImmutableList<String> getExterns() {
        return externs;
    }
}
