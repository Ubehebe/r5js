load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "platform",
    language = JS_VERSION,
    srcs = glob(["*.js"]),
    deps = [
        "//js:boot",
        "//js/eval",
        "//js/io",
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)