load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
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

ts_library(
    name = "value_ts",
    srcs = [
        "value.ts",
    ],
        visibility = [
            "//js:__subpackages__",
        ],
)

closure_ts_library(
    name = "value",
    deps = [
    ":value_ts",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "error_ts",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "error.ts",
    ],
)

closure_ts_library(
    name = "error",
    deps = [
        ":error_ts",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
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
