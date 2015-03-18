package r5js;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.concurrent.Executors;

final class DevServer {

    private static final String JQUERY_URL = "https://code.jquery.com/jquery-1.11.2.min.js";
    private static final String JQCONSOLE_URL = "jqconsole.js";
    private static final String JQCONSOLE_DIV_ID = "jqconsole";
    private static final String JQCONSOLE_BOOT_SCRIPT = String.format(
            "$(function() { r5js.repl.main($('#%s').jqconsole()); });", JQCONSOLE_DIV_ID);
    private static final String JQCONSOLE_CSS = "position: relative; width: 100%; height: 100px;";

    private static final byte[] INDEX = (
            String.format(
            "<!DOCTYPE html>\n"
                    + "<html>\n"
                    + "<head>\n"
                    + "<title>r5js</title>\n"
                    + "<script src=\"%s\"></script>\n" // test runner
                    + "<script src=\"%s\"></script>\n" // repl
                    + "<script src=\"%s\"></script>\n" // JQUERY_URL
                    + "<script src=\"%s\"></script>\n" // JQCONSOLE_URL
                    + "<script>%s</script>" // JQCONSOLE_BOOT_SCRIPT
                    + "</head>\n"
                    + "<body>\n"
                    + "<button onclick=\"r5js.test.main()\">Run Tests</button>\n"
                    + "<div id=\"%s\" " // JQCONSOLE_DIV_ID
                    + "style=\"%s\" />" // JQCONSOLE_CSS
                    + "</body>\n"
                    + "</html>\n",
                    CompilationUnit.HTML5_TEST_RUNNER.getBuildArtifactName(),
                    CompilationUnit.HTML5_REPL.getBuildArtifactName(),
                    JQUERY_URL,
                    JQCONSOLE_URL,
                    JQCONSOLE_BOOT_SCRIPT,
                    JQCONSOLE_DIV_ID,
                    JQCONSOLE_CSS))
            .getBytes();

    private static TargetOutput compiledApp;

    public static void main(String[] args) throws IOException {
        InetSocketAddress address = new InetSocketAddress(8080);
        HttpServer server = HttpServer.create(address, 0);
        server.createContext("/", DevServer::handle);
        server.setExecutor(Executors.newSingleThreadExecutor());
        server.start();
        System.out.printf("DevServer: listening on %d%n", 8080);
    }

    private static void handle(HttpExchange exchange) throws IOException {
        try (OutputStream out = exchange.getResponseBody()) {
            String url = exchange.getRequestURI().toString();
            if ("/".equals(url)) {
                exchange.sendResponseHeaders(200, 0);
                out.write(INDEX);
                return;
            } else if (url.endsWith(JQCONSOLE_URL)) {
                exchange.sendResponseHeaders(200, 0);
                out.write(Files.readAllBytes(Paths.get("jq-console", "jqconsole.min.js")));
                return;
            }
            for (CompilationUnitOutput output : getCompiledJs().outputs) {
                if (url.substring(1).equals(output.getBuildArtifactName())) {
                    exchange.sendResponseHeaders(200, 0);
                    out.write(output.getBytes());
                    return;
                }
            }
            exchange.sendResponseHeaders(404, -1);
        }
    }

    private static synchronized TargetOutput getCompiledJs() throws IOException {
        if (compiledApp == null) {
            compiledApp = Targets.HTML5_ALL.build();
        }
        return compiledApp;
    }
}
