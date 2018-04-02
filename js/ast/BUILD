load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "datum",
    srcs = [
        "boolean.ts",
        "cdr_helper.ts",
        "character.ts",
        "compound_datum.ts",
        "continuable_helper.ts",
        "datum.ts",
        "datum_util.ts",
        "identifier.ts",
        "pair.ts",
        "list.ts",
        "macro.ts",
        "number.ts",
        "proc_call_like.ts",
        "quasiquote.ts",
        "quote.ts",
        "ref.ts",
        "sibling_buffer.ts",
        "simple_datum.ts",
        "string.ts",
        "unquote.ts",
        "unquote_splicing.ts",
        "util.ts",
        "vector.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        ":rename_helper",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/runtime:environment",
        "//js:error",
        "//js:value",
    ],
)

ts_library(
    name = "rename_helper",
    srcs = ["rename_helper.ts"],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js/parse:rename_util",
    ],
)

ts_library(
    name = "type",
    srcs = [
        "type.ts",
        "types.ts",
    ],
    tsconfig = "//:tsconfig.json",
)