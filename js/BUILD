load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "value",
    srcs = [
        "value.js",
    ],
    language = JS_VERSION,
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "error",
    srcs = [
        "error.js",
    ],
    language = JS_VERSION,
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "boot",
    srcs = ["boot.js"],
    language = JS_VERSION,
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        ":error",
        ":value",
        "//js/ast",
        "//js/eval",
        "//js/io",
        "//js/macro",
        "//js/parse",
        "//js/parse:nonterminals",
        "//js/parse:parser",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/read",
        "//js/runtime",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
        "//js/runtime:used_by_parser_impl",
        "//js/scan",
    ],
)
