load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "repl",
    srcs = glob(["*.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js:boot",
        "//js:error",
        "//js/eval:shim",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)