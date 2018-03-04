load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

exports_files([
    "datum_stream.ts",
    "desugarable_rule.ts",
    "grammar.ts",
    "parse_grammar.ts",
    "parse_rule.ts",
    "parse_rule_factory.ts",
    "parser.ts",
    "reader.ts",
    "rule.ts",
])

ts_library(
    name = "nonterminals_ts",
    srcs = [
        "nonterminals.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "nonterminals",
    deps = [
        ":nonterminals_ts"
    ],
)

closure_js_library(
    name = "parser_impl",
    srcs = [
        "parser_impl.js",
    ],
    deps = [
        "//js/macro:shim",
        "//js/runtime:used_by_parser_impl",
    ],
    suppress = [
        "JSC_INVALID_CAST",
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

ts_library(
    name = "rename_util_ts",
    srcs = [
        "rename_util.ts",
    ],
    tsconfig = "//:tsconfig.json",
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
    deps = [
        ":rename_util_ts",
    ],
)

ts_library(
    name = "terminals_ts",
    srcs = [
        "terminals.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "terminals",
    deps = [
        ":terminals_ts",
    ],
)
