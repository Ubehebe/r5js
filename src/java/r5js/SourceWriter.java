package r5js;

import com.google.common.escape.Escaper;
import com.google.common.escape.Escapers;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

public final class SourceWriter {

  private static final Escaper ESCAPER = Escapers.builder()
      .addEscape('"', "\\\"")
      .addEscape('\n', "\\n")
      .addEscape('\\', "\\\\")
      .build();

  private static String format(String contents, String name) {
    String escaped = ESCAPER.escape(contents);
    return String.format("goog.provide('%s');%n/** @const */var %s = \"%s\";%n",
        name, name, escaped);
  }

  public static void main(String[] args) throws IOException {
    String path = args[0];
    String closurizedName = args[1];
    String contents = new String(Files.readAllBytes(Paths.get(path)), StandardCharsets.UTF_8);
    String formatted = format(contents, closurizedName);
    System.out.println(formatted);
  }
}
