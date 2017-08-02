load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "repl",
    language = "ECMASCRIPT6_STRICT",
    srcs = glob(["*.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//src/js:bbom",
    ],
    visibility = [
        "//src/js:__subpackages__",
    ],
)