package r5js;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
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
                    + "<script src=\"%s\"></script>\n" // HTML5_DEV_CLIENT
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
                    CompilationUnits.HTML5_DEV_CLIENT.getBuildArtifactName(),
                    JQUERY_URL,
                    JQCONSOLE_URL,
                    JQCONSOLE_BOOT_SCRIPT,
                    JQCONSOLE_DIV_ID,
                    JQCONSOLE_CSS))
            .getBytes();

    private static final Map<String, byte[]> files = new HashMap<>();

    private static boolean hasCompiled = false;

    public static void main(String[] args) throws IOException {
        InetSocketAddress address = new InetSocketAddress(8080);
        HttpServer server = HttpServer.create(address, 0);
        server.createContext("/", DevServer::handle);
        server.setExecutor(Executors.newSingleThreadExecutor());
        server.start();
        System.out.printf("DevServer: listening on %d%n", 8080);
    }

    private static void handle(HttpExchange exchange) throws IOException {
        maybeCompile();
        String url = exchange.getRequestURI().toString();
        byte[] bytes = files.get(url);
        if (bytes == null) {
            exchange.sendResponseHeaders(404, -1);
            return;
        }
        exchange.sendResponseHeaders(200, 0);
        try (OutputStream out = exchange.getResponseBody()) {
            out.write(bytes);
        }
    }

    private static synchronized void maybeCompile() throws IOException {
        if (hasCompiled) {
            return;
        }

        files.put("/", INDEX);

        files.put("/" + JQCONSOLE_URL,
                Files.readAllBytes(Paths.get("jq-console", "jqconsole.min.js")));

        CompilationUnitOutput worker = CompilationUnits.HTML5_WORKER.compile();
        files.put("/" + worker.getBuildArtifactName(), worker.getBytes());

        CompilationUnitOutput client = CompilationUnits.HTML5_DEV_CLIENT.compile();
        files.put("/" + client.getBuildArtifactName(), client.getBytes());

        hasCompiled = true;
    }
}
