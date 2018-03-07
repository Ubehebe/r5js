load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "matchers",
    testonly = 1,
    visibility = [
        "//js:__subpackages__",
    ],
    srcs = glob(["*.js"]),
    deps = [
        "@closure_tdd//matchers",
        "//js:error",
        "//js/io",
        "//js/eval:shim",
    ],
    suppress = [
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)