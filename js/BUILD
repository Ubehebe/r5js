load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "scheme_sources",
    srcs = [
        "scheme_sources.js",
    ],
    suppress = [
        "JSC_INCORRECT_SHORTNAME_CAPITALIZATION",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
)

ts_library(
    name = "value_ts",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "value.ts",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_ts_library(
    name = "value",
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        ":value_ts",
    ],
)

ts_library(
    name = "error_ts",
    srcs = [
        "error.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "error",
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        ":error_ts",
    ],
)

closure_js_library(
    name = "boot",
    srcs = ["boot.js"],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
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
