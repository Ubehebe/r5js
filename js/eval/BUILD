load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

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
        "//js:boot.ts",
        "//js/macro:ellipsis_transformer.ts",
        "//js/macro:list_like_transformer.ts",
        "//js/macro:macro.ts",
        "//js/macro:macro_id_transformer.ts",
        "//js/macro:rename_helper.ts",
        "//js/macro:subtransformer.ts",
        "//js/macro:template_bindings.ts",
        "//js/macro:transformer.ts",
        "//js/runtime:call_with_current_continuation.ts",
        "//js/runtime:continuation.ts",
        "//js/runtime:dynamic_wind_continuation.ts",
        "//js/runtime:environment.ts",
        "//js/runtime:eof.ts",
        "//js/runtime:errors.ts",
        "//js/runtime:lambda.ts",
        "//js/runtime:nil.ts",
        "//js/runtime:primitive_procedures.ts",
        "//js/runtime:procedure.ts",
        "//js/runtime:top_level_assignment.ts",
        "//js/runtime:top_level_syntax_assignment.ts",
        "//js/runtime:trampoline.ts",
        "//js/runtime:trampoline_helper.ts",
        "//js/runtime:user_defined_procedure.ts",
        "//js/runtime:valutil.ts",
        "//js/runtime:varargs_user_defined_procedure.ts",
    ],
    outs = [
        "boot.ts",
            "errors.ts",
            "primitive_procedures.ts",
            "procedure.ts",
            "trampoline.ts",
        "ellipsis_transformer.ts",
        "list_like_transformer.ts",
        "macro.ts",
        "macro_id_transformer.ts",
        "rename_helper.ts",
        "subtransformer.ts",
        "template_bindings.ts",
        "transformer.ts",
    ],
    cmd = "cp $(SRCS) $(@D)",
)

ts_library(
    name = "eval",
    srcs = [
        "boot.ts",
        "ellipsis_transformer.ts",
        "errors.ts",
        "evaluator.ts",
        "list_like_transformer.ts",
        "macro.ts",
        "macro_id_transformer.ts",
        "pipeline.ts",
        "primitive_procedures.ts",
        "procedure.ts",
        "rename_helper.ts",
        "subtransformer.ts",
        "template_bindings.ts",
        "trampoline.ts",
        "transformer.ts",
    ],
    deps = [
        "//js:error",
        "//js/ast:datum",
        "//js/ast:rename_helper",
        "//js/ast:type",
        "//js/io",
        "//js/parse:nonterminals",
        "//js/parse:parser",
        "//js/parse:parser_impl",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/read",
        "//js/runtime",
        "//js/scan",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_library(
    name = "async_evaluator",
    srcs = [
        "async_evaluator.ts",
    ],
    deps = [
        ":eval",
        "//js:scheme_sources",
        "//js/io",
    ],
    tsconfig = "//:tsconfig.json",
)