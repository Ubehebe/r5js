load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "matchers",
    testonly = 1,
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//src/js:__subpackages__",
    ],
    srcs = glob(["*.js"]),
    deps = [
        "@closure_tdd//matchers",
        "//src/js:bbom",
        "//src/js:error",
    ],
)