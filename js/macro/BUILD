load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

ts_library(
    name = "macro_ts",
    srcs = [
        "ellipsis_transformer.ts",
        "macro_id_transformer.ts",
        "list_like_transformer.ts",
        "rename_helper.ts",
        "template_bindings.ts",
        "in_memory_input_port.ts",
        "port_manager.ts",
        "reader.ts",
        "rule.ts",
        "rule_factory.ts",
        "datum_stream.ts",
        "desugarable_rule.ts",
        "grammar.ts",
        "parser.ts",
        "subtransformer.ts",
    ],
    deps = [
        "//js/ast:datum_ts",
        "//js:error_ts",
        "//js/parse:nonterminals_ts",
        "//js/parse:rename_util_ts",
        "//js/parse:terminals_ts",
        "//js/io:io_ts",
        "//js/scan:scan_ts",
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
        "//js/parse:datum_stream.ts",
        "//js/parse:desugarable_rule.ts",
#        "//js/parse:grammar.ts",
        "//js/parse:parser.ts",
        "//js/parse:rule.ts",
        "//js/read:grammar.ts",
        "//js/read:reader.ts",
        "//js/read:rule.ts",
        "//js/read:rule_factory.ts",
    ],
    outs = [
        "in_memory_input_port.ts",
        "port_manager.ts",
        "reader.ts",
        "rule.ts",
        "rule_factory.ts",
        "datum_stream.ts",
        "desugarable_rule.ts",
        "grammar.ts",
        "parser.ts",
    ],
    cmd = "cp $(SRCS) $(@D)",
)

closure_ts_library(
    name = "shim",
    deps = [
        ":macro_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    visibility = ["//js:__subpackages__"],
)

closure_js_library(
    name = "macro",
    srcs = glob(["*.js"]),
    deps = [
        ":shim",
        "@io_bazel_rules_closure//closure/library",
        "//js:error",
        "//js/parse:rename_util",
        "//js/runtime:ienvironment",
        "//js:value",
    ],
    visibility = ["//js:__subpackages__"],
    suppress = [
            "JSC_INVALID_CAST",
            "JSC_TYPE_MISMATCH",
            "JSC_UNDEFINED_VARIABLE",
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)
