load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "ast",
    visibility = [
        "//js:__subpackages__",
    ],
    exports = [
        ":boolean",
        ":cdr_helper",
        ":character",
        ":compound_datum",
        ":continuable_helper",
        ":datum",
        ":datum_util",
        ":identifier",
        ":ipair",
        ":list",
        ":literal",
        ":macro",
        ":number",
        ":quasiquote",
        ":quote",
        ":ref",
        ":rename_helper",
        ":sibling_buffer",
        ":simple_datum",
        ":string",
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
    name = "boolean",
    srcs = ["boolean.js"],
    deps = [
        ":simple_datum",
    ],
)

closure_js_library(
    name = "cdr_helper",
    srcs = ["cdr_helper.js"],
    deps = [":datum"],
)

closure_js_library(
    name = "character",
    srcs = ["character.js"],
    deps = [":simple_datum"],
)

closure_js_library(
    name = "compound_datum",
    srcs = ["compound_datum.js"],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":cdr_helper",
        ":datum",
        ":identifier",
        ":rename_helper",
        ":sibling_buffer",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
    ],
)

closure_js_library(
    name = "continuable_helper",
    srcs = [
        "continuable_helper.js",
    ],
    deps = [
        "//js/runtime:proc_call_like",
    ],
)

closure_js_library(
    name = "datum",
    srcs = ["datum.js"],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":rename_helper",
        "//js:error",
        "//js:value",
        "//js/parse:nonterminals",
        "//js/parse:terminals",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
    ],
)

closure_js_library(
    name = "datum_util",
    srcs = [
        "datum_util.js",
    ],
    suppress = [
        "JSC_PRIMITIVE_OBJECT",
    ],
    deps = [
        ":boolean",
        ":datum",
        ":identifier",
        ":number",
        "//js:error",
    ],
)

closure_js_library(
    name = "identifier",
    srcs = ["identifier.js"],
    deps = [
        ":simple_datum",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/runtime:proc_call_like",
    ],
)

closure_js_library(
    name = "ipair",
    srcs = [
        "ipair.js",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
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
        ":cdr_helper",
        ":compound_datum",
        ":datum",
        ":ipair",
        ":sibling_buffer",
        "//js:error",
    ],
)

closure_js_library(
    name = "literal",
    srcs = ["literal.js"],
    deps = [":datum"],
)

closure_js_library(
    name = "macro",
    srcs = ["macro.js"],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":simple_datum",
    ],
)

closure_js_library(
    name = "number",
    srcs = [
        "number.js",
    ],
    deps = [
        ":simple_datum",
    ],
)

closure_js_library(
    name = "quasiquote",
    srcs = ["quasiquote.js"],
    deps = [
        ":compound_datum",
        ":continuable_helper",
        ":datum",
        ":identifier",
        ":quote",
        ":unquote",
        ":unquote_splicing",
        "//js/parse:nonterminals",
        "//js/parse:terminals",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
    ],
)

closure_js_library(
    name = "quote",
    srcs = ["quote.js"],
    deps = [
        ":compound_datum",
        ":datum",
        ":datum_util",
        ":identifier",
        ":ipair",
        ":list",
        ":ref",
        ":unspecified_value",
        "//js:error",
        "//js/parse:terminals",
        "//js/runtime:proc_call_like",
    ],
)

closure_js_library(
    name = "ref",
    srcs = ["ref.js"],
    deps = [
        ":datum",
        ":simple_datum",
    ],
)

closure_js_library(
    name = "rename_helper",
    srcs = ["rename_helper.js"],
    deps = [
        "//js/parse:rename_util",
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

closure_js_library(
    name = "simple_datum",
    srcs = ["simple_datum.js"],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":datum",
        ":literal",
    ],
)

closure_js_library(
    name = "string",
    srcs = ["string.js"],
    deps = [
        ":simple_datum",
    ],
)

closure_js_library(
    name = "type",
    srcs = ["type.js"],
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
    deps = [
        ":compound_datum",
        ":datum",
        ":identifier",
        ":list",
        ":sibling_buffer",
        ":simple_datum",
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
