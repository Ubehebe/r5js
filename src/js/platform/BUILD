load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "platform",
    language = "ECMASCRIPT6_STRICT",
    srcs = glob(["*.js"]),
    deps = [
        "//src/js:bbom",
        "//src/scm:PROCEDURES",
        "//src/scm:SYNTAX",
    ],
    visibility = [
        "//src/js:__subpackages__",
    ],
)