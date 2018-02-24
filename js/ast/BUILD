load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "ast",
    exports = [
        ":datum",
        ":quasiquote",
        ":rename_helper",
        ":type",
        ":unquote_splicing",
        ":util",
        ":vector",
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
        "quote.ts",
        "ref.ts",
        "sibling_buffer.ts",
        "simple_datum.ts",
        "string.ts",
        "unquote.ts",
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

closure_js_library(
    name = "quasiquote",
    srcs = ["quasiquote.js"],
    suppress = [
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":datum",
        ":unquote_splicing",
        "//js/parse:terminals",
        "//js/runtime:ienvironment",
    ],
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

closure_js_library(
    name = "unquote_splicing",
    srcs = ["unquote_splicing.js"],
    deps = [
        ":datum",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "util",
    srcs = ["util.js"],
    suppress = [
        "JSC_INVALID_CAST",
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":datum",
        "//js/parse:terminals",
    ],
)

closure_js_library(
    name = "vector",
    srcs = [
        "vector.js",
    ],
    suppress = [
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":datum",
    ],
)
