load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "eval",
    srcs = glob(["*.js"], exclude = ["evaluator_impl.js"]),
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

closure_js_library(
    name = "impl",
    language = JS_VERSION,
    srcs = glob(["evaluator_impl.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        ":eval",
        "//js:boot",
        "//js:scheme_sources",
        "//js/io",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)