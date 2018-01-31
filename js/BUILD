load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "scheme_sources",
    srcs = [
        "scheme_sources.js",
    ],
    deps = [
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    suppress = [
        "JSC_INCORRECT_SHORTNAME_CAPITALIZATION",
    ]
)

closure_js_library(
    name = "value",
    srcs = [
        "value.js",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "error",
    srcs = [
        "error.js",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "boot",
    srcs = ["boot.js"],
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
    suppress = [
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)
