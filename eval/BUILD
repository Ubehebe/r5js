load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//:__subpackages__",
    ],
)

ts_library(
    name = "eval",
    srcs = [
        "evaluator.ts",
        "pipeline.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//ast:datum",
        "//ast:rename_helper",
        "//ast:type",
        "//base:error",
        "//base:value",
        "//io",
        "//parse:nonterminals",
        "//parse:parser",
        "//parse:parser_impl",
        "//parse:rename_util",
        "//parse:terminals",
        "//read",
        "//runtime",
        "//runtime:environment",
        "//runtime:primitive_procedures",
        "//runtime:trampoline",
        "//scan",
    ],
)

ts_library(
    name = "boot",
    srcs = [
        "boot.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":eval",
        "//io",
        "//parse:parser_impl",
        "//read",
        "//runtime",
        "//runtime:environment",
        "//runtime:primitive_procedures",
        "//runtime:trampoline",
        "//scan",
    ],
)

ts_library(
    name = "async_evaluator",
    srcs = [
        "async_evaluator.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":boot",
        ":eval",
        "//io",
        "//scm",
    ],
)
