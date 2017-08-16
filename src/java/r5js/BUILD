java_library(
    name = "r5js",
    srcs = glob(["*.java"]),
    deps = [
        "@my_guava//jar",
    ],
    visibility = [
        "//:__subpackages__",
    ],
)

java_binary(
    name = "SourceWriter",
    main_class = "r5js.SourceWriter",
    runtime_deps = [
        ":r5js",
    ],
    visibility = [
        "//visibility:public",
    ],
)