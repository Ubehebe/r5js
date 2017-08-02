java_library(
    name = "r5js",
    srcs = glob(["*.java"]),
    deps = [
        "@closure_compiler_externs//jar",
        "@my_closure_compiler//jar",
        "@my_guava//jar",
        "@truth//jar",
        "@junit//jar",
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
