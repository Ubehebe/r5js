load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

exports_files([
    "datum_stream.ts",
    "desugarable_rule.ts",
    "grammar.ts",
    "parse_grammar.ts",
    "parse_rule.ts",
    "parse_rule_factory.ts",
    "parser.ts",
    "parser_impl.ts",
    "reader.ts",
    "rule.ts",
])

ts_library(
    name = "nonterminals",
    srcs = [
        "nonterminals.ts",
    ],
    tsconfig = "//:tsconfig.json",
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