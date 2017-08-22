load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "eval",
    srcs = glob(["**/*.js"], exclude = ["valutil.js"]),
    visibility = ["//js:__subpackages__"],
    language = JS_VERSION,
    deps = [
        "@io_bazel_rules_closure//closure/library",
        ":valutil",
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

# TODO used by runtime and eval. Move to runtime?
closure_js_library(
    name = "valutil",
    srcs = [
        "valutil.js",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    language = JS_VERSION,
    deps = [
        "//js/ast",
        "//js/io",
        "//js/parse:terminals",
        "//js/runtime:used_by_parser_impl",
        "//js:value",
    ],
)
