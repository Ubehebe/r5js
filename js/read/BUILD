load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "read",
    srcs = glob(["*.js"]),
    language = JS_VERSION,
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js/ast",
        "//js:error",
        "//js/parse:nonterminals",
        "//js/parse:terminals",
        "//js/scan",
    ],
    visibility = ["//js:__subpackages__"],
)
