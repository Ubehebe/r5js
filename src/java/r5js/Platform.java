package r5js;

import com.google.common.collect.ImmutableList;

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
