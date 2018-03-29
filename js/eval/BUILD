load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "eval",
    srcs = [
        "evaluator.ts",
        "pipeline.ts",
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
        "//js/runtime:primitive_procedures",
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
        "//js:boot",
        "//js:scheme_sources",
        "//js/io",
    ],
    tsconfig = "//:tsconfig.json",
)