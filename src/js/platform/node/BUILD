load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_binary", "closure_js_library")

closure_js_library(
    name = "node",
    language = "ECMASCRIPT6_STRICT",
    srcs = glob(["*.js"]),
    deps = [
        "//:node_externs",
        "//src/js/platform",
        "//src/js/platform/common",
        "//src/js/repl",
        "//src/js:bbom",
        "@io_bazel_rules_closure//closure/library",
    ],
    visibility = [
        "//src/js:__subpackages__",
    ],
)

closure_js_binary(
    name = "bin",
    deps = [
        ":node",
    ],
    entry_points = [
        "goog:r5js.platform.node.repl",
    ],
)

closure_js_binary(
    name = "tests",
    testonly = 1,
    deps = [
        ":node",
        "//src/js/test:lib",
    ],
    entry_points = [
        "goog:r5js.test.main",
    ],
)