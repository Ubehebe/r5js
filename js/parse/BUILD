load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "nonterminals",
    srcs = [
        "nonterminals.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_library(
    name = "parse",
    srcs = [
        "datum_stream.ts",
        "parse_grammar.ts",
        "parse_rule.ts",
        "parse_rule_factory.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":nonterminals",
        ":terminals",
        "//js/ast:datum",
        "//js/runtime:environment",
    ],
)

ts_library(
    name = "parser",
    srcs = [
        "parser.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":nonterminals",
        "//js/ast:datum",
    ],
)

ts_library(
    name = "parser_impl",
    srcs = [
        "parser_impl.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":nonterminals",
        ":parse",
        ":parser",
        ":rename_util",
        ":terminals",
        "//js:error",
        "//js/ast:datum",
        "//js/ast:rename_helper",
        "//js/macro",
        "//js/runtime",
        "//js/runtime:environment",
    ],
)

ts_library(
    name = "rename_util",
    srcs = [
        "rename_util.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":terminals",
    ],
)

ts_library(
    name = "terminals",
    srcs = [
        "terminals.ts",
    ],
    tsconfig = "//:tsconfig.json",
)
