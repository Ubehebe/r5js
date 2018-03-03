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
        ":nonterminals",
        ":parser_impl",
        ":rename_util",
        ":shim",
        ":terminals",
    ],
)

# Workaround to avoid duplication of the AST classes in js/read execroot.
# Instead of polluting the commit history by moving in_memory_input_port.ts into this package,
# just copy it here temporarily via a genrule. TODO: remove.
genrule(
    name = "ugh",
    srcs = [
        "//js/io:in_memory_input_port.ts",
        "//js/io:port_manager.ts",
        "//js/read:grammar.ts",
        "//js/read:reader.ts",
        "//js/read:rule.ts",
        "//js/read:rule_factory.ts",
    ],
    outs = [
        "grammar.ts",
        "in_memory_input_port.ts",
        "port_manager.ts",
        "reader.ts",
        "rule.ts",
        "rule_factory.ts",
    ],
    cmd = "cp $(SRCS) $(@D)",
)

ts_library(
    name = "parse_ts",
    srcs = [
        "datum_stream.ts",
        "desugarable_rule.ts",
        "grammar.ts",
        "in_memory_input_port.ts",
        "parser.ts",
        "port_manager.ts",
        "reader.ts",
        "rule.ts",
        "rule_factory.ts",
    ],
    deps = [
        ":nonterminals_ts",
        ":terminals_ts",
        "//js/ast:datum_ts",
        "//js/io:io_ts",
        "//js/scan:scan_ts",
        "//js:error_ts",
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
        ":rule_factory",
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
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
        "JSC_TYPE_MISMATCH",
        "JSC_HIDDEN_SUPERCLASS_PROPERTY_MISMATCH",
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
