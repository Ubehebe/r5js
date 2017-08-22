load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "io",
    srcs = glob(["*.js"]),
    deps = [
        "//js/ast",
        "//js/read",
        "//js/scan",
        "//js:value",
    ],
    language = JS_VERSION,
    visibility = ["//js:__subpackages__"],
)
