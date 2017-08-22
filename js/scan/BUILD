load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "scan",
    srcs = glob(["*.js"]),
    deps = [
        "//js:error",
        "//js/ast",
        "//js/parse:terminals",
    ],
    suppress = [
        "JSC_PRIMITIVE_OBJECT",
    ],
    language = JS_VERSION,
    visibility = ["//js:__subpackages__"],
)
