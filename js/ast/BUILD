load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "datum_ts",
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
        ":rename_helper_ts",
        "//js/parse:nonterminals_ts",
        "//js/parse:rename_util_ts",
        "//js/parse:terminals_ts",
        "//js/runtime:ienvironment_ts",
        "//js:error_ts",
        "//js:value_ts",
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

closure_ts_library(
    name = "datum",
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_MISSING_MODULE_OR_PROVIDE",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [":datum_ts"],
)

ts_library(
    name = "rename_helper_ts",
    srcs = ["rename_helper.ts"],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js/parse:rename_util_ts",
    ],
)

closure_ts_library(
    name = "rename_helper",
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
    ],
    deps = [
        ":rename_helper_ts",
    ],
)

ts_library(
    name = "type_ts",
    srcs = ["type.ts"],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "type",
    deps = [":type_ts"],
)