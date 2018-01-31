load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "parse",
    srcs = glob(["*.js"], exclude = [
        "nonterminals.js",
        "parser.js",
        "rename_util.js",
        "terminals.js",
    ]),
    deps = [
        "//js/ast",
        "//js:error",
        "//js/macro",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
        "//js/runtime:used_by_parser_impl",
        ":nonterminals",
        ":terminals",
        ":parser",
        ":rename_util",
    ],
    visibility = ["//js:__subpackages__"],
    suppress = [
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "nonterminals",
    srcs = [
        "nonterminals.js",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "parser",
    srcs = [
        "parser.js",
    ],
    deps = [
        "//js/ast",
        ":nonterminals",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "rename_util",
    srcs = [
        "rename_util.js",
    ],
    deps = [
        ":terminals",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "terminals",
    srcs = [
        "terminals.js",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)


