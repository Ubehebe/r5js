load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "repl",
    language = JS_VERSION,
    srcs = glob(["*.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js/ast",
        "//js:boot",
        "//js:error",
        "//js/eval",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)