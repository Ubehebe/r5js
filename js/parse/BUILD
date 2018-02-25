load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "parse",
    srcs = [],
    exports = [
        ":desugarable_rule",
        ":grammar",
        ":nonterminals",
        ":parser",
        ":parser_impl",
        ":rename_util",
        ":shim",
        ":terminals",
    ],
)

ts_library(
    name = "parse_ts",
    srcs = [
        "datum_stream.ts",
        "rule.ts",
    ],
    deps = [
        "//js/ast:datum_ts",
    ],
)

closure_ts_library(
    name = "shim",
    deps = [
        ":parse_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_UNKNOWN_EXPR_TYPE",
    ]
)

closure_js_library(
    name = "desugarable_rule",
    srcs = [
        "desugarable_rule.js",
    ],
    deps = [
        ":shim",
    ],
)

closure_js_library(
    name = "grammar",
    srcs = [
        "grammar.js",
    ],
    deps = [
        ":nonterminals",
        ":shim",
    ]
)

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
    name = "parser",
    srcs = [
        "parser.js",
    ],
    deps = [
        "//js/ast",
    ],
)

closure_js_library(
    name = "parser_impl",
    srcs = [
        "parser_impl.js",
    ],
    deps = [
        ":grammar",
        ":parser",
        ":shim",
        ":rule_factory",
        "//js/ast",
        "//js/macro",
        "//js/runtime:used_by_parser_impl",
    ],
    suppress = [
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

closure_js_library(
    name = "rule_factory",
    srcs = [
        "rule_factory.js",
    ],
    deps = [
        ":desugarable_rule",
        ":grammar",
        ":shim",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
        "JSC_TYPE_MISMATCH",
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
