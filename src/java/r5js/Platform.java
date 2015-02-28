package r5js;

import com.google.common.collect.ImmutableList;

import java.nio.file.Path;

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

    boolean relevant(Path path) {
        return path.getFileName().toString().endsWith(".js")
                && (path.startsWith("closure-library")
                || (path.startsWith("src/js") && isRelevantSourcePath(path)));
    }

    private boolean isRelevantSourcePath(Path path) {
        if (!path.startsWith("src/js/platform")) {
            return true;
        }

        Path parent = path.getParent();
        return parent.endsWith("platform") || parent.endsWith(closureDefineName);
    }
}
