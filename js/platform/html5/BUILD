load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "html5",
    srcs = glob(["*.js"]),
    language = JS_VERSION,
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js:boot",
        "//js/platform",
        "//js/repl",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)