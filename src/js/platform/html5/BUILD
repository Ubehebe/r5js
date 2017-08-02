load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "html5",
    srcs = glob(["*.js"]),
    language = "ECMASCRIPT6_STRICT",
    deps = [
        "//src/js:bbom",
    ],
    visibility = [
        "//src/js:__subpackages__",
    ],
)