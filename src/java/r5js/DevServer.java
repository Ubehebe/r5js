package r5js;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.util.concurrent.Executors;

final class DevServer {

    private static final byte[] INDEX = (
            "<!DOCTYPE html>\n"
                    + "<html>\n"
                    + "<head>\n"
                    + "<title>r5js tests (compiled)</title>\n"
                    + "<script src=\""
                    + CompilationUnit.HTML5_CLIENT.getBuildArtifactName()
                    +"\"></script>\n"
                    + "</head>\n"
                    + "<body>\n"
                    + "<button onclick=\"r5js.test.main()\">Run Tests</button>\n"
                    + "</body>\n"
                    + "</html>\n")
            .getBytes();

    private static CompilationResult compiledApp;

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

    private static synchronized CompilationResult getCompiledJs() throws IOException {
        if (compiledApp == null) {
            compiledApp = Targets.HTML5_TESTS.build();
        }
        return compiledApp;
    }
}
