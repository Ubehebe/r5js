load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "node",
    language = JS_VERSION,
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