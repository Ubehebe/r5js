load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "node",
    language = "ECMASCRIPT6_STRICT",
    srcs = glob(["*.js"]),
    deps = [
        "//:node_externs",
        "//js/eval",
        "//js/io",
        "//js/platform",
        "//js/platform/common",
        "//js/repl",
        "//js:boot",
        "@io_bazel_rules_closure//closure/library",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)