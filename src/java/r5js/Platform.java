package r5js;

enum Platform {
    ANDROID,
    HTML5,
    NASHORN,
    NODE;

    @Override
    public String toString() {
        return super.toString().toLowerCase();
    }
}
