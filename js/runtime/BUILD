load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
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
        "//js/macro:ellipsis_transformer.ts",
        "//js/macro:list_like_transformer.ts",
        "//js/macro:macro.ts",
        "//js/macro:macro_id_transformer.ts",
        "//js/macro:rename_helper.ts",
        "//js/macro:subtransformer.ts",
        "//js/macro:template_bindings.ts",
        "//js/macro:transformer.ts",
        "//js/parse:datum_stream.ts",
        "//js/parse:desugarable_rule.ts",
        "//js/parse:parse_rule_factory.ts",
        "//js/parse:parse_grammar.ts",
        "//js/parse:parser.ts",
        "//js/parse:parser_impl.ts",
        "//js/parse:parse_rule.ts",
        "//js/read:grammar.ts",
        "//js/read:reader.ts",
        "//js/read:rule.ts",
        "//js/read:rule_factory.ts",
    ],
    outs = [
        "datum_stream.ts",
        "desugarable_rule.ts",
        "ellipsis_transformer.ts",
        "grammar.ts",
        "in_memory_input_port.ts",
        "list_like_transformer.ts",
        "macro.ts",
        "macro_id_transformer.ts",
        "parse_grammar.ts",
        "parse_rule.ts",
        "parse_rule_factory.ts",
        "parser.ts",
        "parser_impl.ts",
        "port_manager.ts",
        "reader.ts",
        "rename_helper.ts",
        "rule.ts",
        "rule_factory.ts",
        "subtransformer.ts",
        "template_bindings.ts",
        "transformer.ts",
    ],
    cmd = "cp $(SRCS) $(@D)",
)

ts_library(
    name = "runtime_ts",
    srcs = [
        "assignment.ts",
        "branch.ts",
        "call_with_current_continuation.ts",
        "continuation.ts",
        "datum_stream.ts",
        "desugarable_rule.ts",
        "dynamic_wind_continuation.ts",
        "ellipsis_transformer.ts",
        "eof.ts",
        "environment.ts",
        "errors.ts",
        "grammar.ts",
        "in_memory_input_port.ts",
        "lambda.ts",
        "list_like_transformer.ts",
        "macro.ts",
        "macro_id_transformer.ts",
        "parse_grammar.ts",
        "parse_rule.ts",
        "parse_rule_factory.ts",
        "parser.ts",
        "parser_impl.ts",
        "port_manager.ts",
        "primitive_procedures.ts",
        "proc_call.ts",
        "procedure.ts",
        "procspec.ts",
        "reader.ts",
        "rename_helper.ts",
        "rule.ts",
        "rule_factory.ts",
        "subtransformer.ts",
        "template_bindings.ts",
        "top_level_assignment.ts",
        "top_level_syntax_assignment.ts",
        "trampoline.ts",
        "trampoline_helper.ts",
        "transformer.ts",
        "user_defined_procedure.ts",
        "valutil.ts",
        "varargs_user_defined_procedure.ts",
    ],
    deps = [
        "//js:error_ts",
        "//js/ast:datum_ts",
        "//js/ast:rename_helper_ts",
        "//js/ast:type_ts",
        "//js/io:io_ts",
        "//js/parse:nonterminals_ts",
        "//js/parse:rename_util_ts",
        "//js/parse:terminals_ts",
        "//js/scan:scan_ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "shim",
    deps = [
        ":runtime_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_IMPLICITLY_NULLABLE_JSDOC",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_NULLABLE_RETURN_WITH_NAME",
        "JSC_UNKNOWN_EXPR_TYPE",
    ]
)

ts_library(
    name = "ienvironment_ts",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "ienvironment.ts",
    ],
    deps = [
        "//js:value_ts",
    ],
)

closure_ts_library(
    name = "ienvironment",
    deps = [
        ":ienvironment_ts",
    ],
)