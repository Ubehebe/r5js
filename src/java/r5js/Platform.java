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

    ANDROID(ImmutableList.of("custom-externs/android.js")),
    HTML5(ImmutableList.<String>of()),
    NASHORN(ImmutableList.<String>of()),
    NODE(ImmutableList.of(
            "externs/core.js",
            "externs/process.js",
            "externs/readline.js"));

    private final ImmutableList<String> externs;

    Platform(ImmutableList<String> externs) {
        this.externs = externs;
    }

    public ImmutableList<String> getExterns() {
        return externs;
    }
}
