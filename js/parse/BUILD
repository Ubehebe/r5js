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
        ":datum_stream",
        ":desugarable_rule",
        ":grammar",
        ":nonterminals",
        ":parser",
        ":parser_impl",
        ":rename_util",
        ":terminals",
    ],
)

closure_js_library(
    name = "datum_stream",
    srcs = [
        "datum_stream.js",
    ],
    deps = [
        "//js/ast",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "desugarable_rule",
    srcs = [
        "desugarable_rule.js",
    ],
    deps = [
        ":rule",
    ],
)

closure_js_library(
    name = "grammar",
    srcs = [
        "grammar.js",
    ],
    deps = [
        ":nonterminals",
        ":rule",
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
        ":datum_stream",
        ":grammar",
        ":parser",
        ":rule",
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
    name = "rule",
    srcs = [
        "rule.js",
    ],
    deps = [
        "//js/ast",
        ":datum_stream",
    ]
)

closure_js_library(
    name = "rule_factory",
    srcs = [
        "rule_factory.js",
    ],
    deps = [
        ":desugarable_rule",
        ":grammar",
        ":rule",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
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
