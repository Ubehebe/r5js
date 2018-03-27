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
    name = "nonterminals_ts",
    srcs = [
        "nonterminals.ts",
    ],
    tsconfig = "//:tsconfig.json",
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

ts_library(
    name = "terminals_ts",
    srcs = [
        "terminals.ts",
    ],
    tsconfig = "//:tsconfig.json",
)