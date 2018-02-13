load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "parse",
    srcs = glob(
        ["*.js"],
        exclude = [
            "nonterminals.js",
            "parser.js",
            "rename_util.ts",
            "terminals.ts",
        ],
    ),
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    visibility = ["//js:__subpackages__"],
    deps = [
        ":nonterminals",
        ":parser",
        ":rename_util",
        ":terminals",
        "//js:error",
        "//js/ast",
        "//js/macro",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
        "//js/runtime:used_by_parser_impl",
    ],
)

ts_library(
    name = "nonterminals_ts",
    srcs = [
        "nonterminals.ts",
    ],
)

closure_ts_library(
    name = "nonterminals",
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        ":nonterminals_ts",
    ],
)

closure_js_library(
    name = "parser",
    srcs = [
        "parser.js",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        ":nonterminals",
        "//js/ast",
    ],
)

ts_library(
    name = "rename_util_ts",
    srcs = [
        "rename_util.ts",
    ],
    deps = [
        ":terminals_ts",
    ],
)

closure_ts_library(
    name = "rename_util",
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        ":rename_util_ts",
    ],
)

ts_library(
    name = "terminals_ts",
    srcs = [
        "terminals.ts",
    ],
)

closure_ts_library(
    name = "terminals",
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        ":terminals_ts",
    ],
)
