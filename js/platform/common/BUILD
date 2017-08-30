load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "common",
    language = JS_VERSION,
    srcs = glob(["*.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js:boot",
        "//js:scheme_sources",
        "//js/eval",
        "//js/io",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)