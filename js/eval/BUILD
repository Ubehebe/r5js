load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "eval",
    srcs = glob(["*.js"]),
    visibility = ["//js:__subpackages__"],
    language = JS_VERSION,
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js/ast",
        "//js:error",
        "//js/io",
        "//js/parse",
        "//js/parse:nonterminals",
        "//js/read",
        "//js/scan",
        "//js/runtime",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
        "//js:value",
    ],
)
