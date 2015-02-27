package r5js;

import com.google.common.collect.ImmutableList;
import com.google.javascript.jscomp.SourceFile;

enum Platform {
    ANDROID("android", ImmutableList.of("r5js.test.main")),
    HTML5("html5", ImmutableList.of("r5js.test.main")),
    NODE("node", ImmutableList.of("r5js.test.main"));

    final String closureDefineName;
    final ImmutableList<String> closureEntryPoints;
    final String jsSrcDir;

    Platform(String closureDefineName, ImmutableList<String> closureEntryPoints) {
        this.closureDefineName = closureDefineName;
        this.closureEntryPoints = closureEntryPoints;
        this.jsSrcDir = "src/js/" + closureDefineName;
    }

    boolean relevant(SourceFile file) {
        String path = file.getOriginalPath();
        // TODO bl not quite right
        if (!path.contains("src/js/platform/")) {
            return true;
        }
        return path.contains(jsSrcDir);
    }
}
