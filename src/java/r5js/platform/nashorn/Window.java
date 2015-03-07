package r5js.platform.nashorn;

public final class Window {
    public void setTimeout(long callback, Object delay) {
        System.out.printf("%d %s", callback, delay);
    }
}
