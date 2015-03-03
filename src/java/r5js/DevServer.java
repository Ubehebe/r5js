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
                    + "<script src=\"test.js\"></script>\n"
                    + "</head>\n"
                    + "<body>\n"
                    + "<button onclick=\"r5js.test.main()\">Run Tests</button>\n"
                    + "</body>\n"
                    + "</html>\n")
            .getBytes();

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
            switch (exchange.getRequestURI().toString()) {
                case "/":
                    exchange.sendResponseHeaders(200, 0);
                    out.write(INDEX);
                    break;
                case "/test.js":
                    exchange.sendResponseHeaders(200, 0);
                    out.write("var x = 42;".getBytes());
                    break;
                default:
                    exchange.sendResponseHeaders(404, -1);
                    break;
            }
        }
    }
}
