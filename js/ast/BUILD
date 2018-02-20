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
        ":compound_datum",
        ":datum",
        ":ipair",
        ":list",
        ":macro",
        ":quasiquote",
        ":quote",
        ":rename_helper",
        ":sibling_buffer",
        ":type",
        ":unquote",
        ":unquote_splicing",
        ":unspecified_value",
        ":util",
        ":vacuous_program",
        ":vector",
    ],
)

closure_js_library(
    name = "compound_datum",
    srcs = ["compound_datum.js"],
    suppress = [
        "JSC_INVALID_CAST",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":datum",
        ":rename_helper",
        ":sibling_buffer",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
    ],
)

ts_library(
    name = "datum_ts",
    srcs = [
        "boolean.ts",
        "cdr_helper.ts",
        "character.ts",
        "continuable_helper.ts",
        "datum.ts",
        "datum_util.ts",
        "identifier.ts",
        "number.ts",
        "ref.ts",
        "simple_datum.ts",
        "string.ts",
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

ts_library(
    name = "ipair_ts",
    srcs = ["ipair.ts"],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js:value_ts",
    ],
)

closure_ts_library(
    name = "ipair",
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = ["ipair_ts"],
)

closure_js_library(
    name = "list",
    srcs = [
        "list.js",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":compound_datum",
        ":datum",
        ":ipair",
        ":sibling_buffer",
        "//js:error",
    ],
)

closure_js_library(
    name = "macro",
    srcs = ["macro.js"],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":datum",
    ],
)

closure_js_library(
    name = "quasiquote",
    srcs = ["quasiquote.js"],
    suppress = [
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":compound_datum",
        ":datum",
        ":quote",
        ":unquote",
        ":unquote_splicing",
        "//js/parse:nonterminals",
        "//js/parse:terminals",
        "//js/runtime:ienvironment",
    ],
)

closure_js_library(
    name = "quote",
    srcs = ["quote.js"],
    deps = [
        ":compound_datum",
        ":datum",
        ":ipair",
        ":list",
        ":unspecified_value",
        "//js:error",
        "//js/parse:terminals",
    ],
    suppress = [
        "JSC_INVALID_CAST",
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

closure_js_library(
    name = "sibling_buffer",
    srcs = [
        "sibling_buffer.js",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":datum",
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
    name = "unquote",
    srcs = ["unquote.js"],
    deps = [
        ":compound_datum",
        ":datum",
    ],
)

closure_js_library(
    name = "unquote_splicing",
    srcs = ["unquote_splicing.js"],
    deps = [
        ":compound_datum",
        ":datum",
    ],
)

closure_js_library(
    name = "unspecified_value",
    srcs = [
        "unspecified_value.js",
    ],
    deps = [
        ":datum",
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
        ":compound_datum",
        ":list",
        ":sibling_buffer",
        ":datum",
        "//js/parse:nonterminals",
        "//js/parse:terminals",
    ],
)

closure_js_library(
    name = "vacuous_program",
    srcs = [
        "vacuous_program.js",
    ],
    deps = [
        ":datum",
    ],
)

closure_js_library(
    name = "vector",
    srcs = [
        "vector.js",
    ],
    deps = [
        ":compound_datum",
        ":datum",
    ],
)
