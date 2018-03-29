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
        "//js/runtime:errors.ts",
        "//js/runtime:primitive_procedures.ts",
        "//js/runtime:procedure.ts",
        "//js/runtime:trampoline.ts",
    ],
    outs = [
        "boot.ts",
            "errors.ts",
            "primitive_procedures.ts",
            "procedure.ts",
    ],
    cmd = "cp $(SRCS) $(@D)",
)

ts_library(
    name = "eval",
    srcs = [
        "boot.ts",
        "errors.ts",
        "evaluator.ts",
        "pipeline.ts",
        "primitive_procedures.ts",
        "procedure.ts",
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
        "//js/runtime:trampoline",
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