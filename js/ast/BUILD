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
        "ipair.ts",
        "list.ts",
        "macro.ts",
        "number.ts",
        "quasiquote.ts",
        "quote.ts",
        "ref.ts",
        "sibling_buffer.ts",
        "simple_datum.ts",
        "string.ts",
        "token_stream.ts",
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
        "//js/runtime:ienvironment",
        "//js:error",
        "//js:value",
    ],
)

# Workaround to avoid duplication of the AST classes in js/scan execroot.
# Instead of polluting the commit history by moving token_stream.ts into this package,
# just copy it here temporarily via a genrule. TODO: remove.
genrule(
    name = "ugh",
    srcs = [
        "//js/scan:token_stream.ts",
    ],
    outs = [
        "token_stream.ts",
    ],
    cmd = "cp $(<) $(@)",
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