load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

SRCS_USED_BY_PARSER_IMPL = [
    "assignment.js",
    "branch.js",
    "continuation.js",
    "eof.js",
    "errors.js",
    "lambda.js",
    "procedure.js",
    "proc_call.js",
    "top_level_assignment.js",
    "top_level_syntax_assignment.js",
    "user_defined_procedure.js",
    "varargs_user_defined_procedure.js",
]

closure_js_library(
    name = "used_by_parser_impl",
    srcs = SRCS_USED_BY_PARSER_IMPL,
    deps = [
        "//js:error",
        "//js/ast:type",
        "//js/macro",
        "//js/parse:terminals",
        "//js:value",
        ":ienvironment",
    ],
    suppress = [
        "JSC_INVALID_CAST",
        "JSC_MUST_BE_PRIVATE",
        "JSC_MISSING_RETURN_JSDOC",
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "runtime",
    srcs = glob(["*.js"], exclude = [
        "ienvironment.ts",
    ] + SRCS_USED_BY_PARSER_IMPL),
    deps = [
        ":used_by_parser_impl",
        "//js:error",
        "//js/io",
        "//js/macro",
        "//js/parse",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/runtime:ienvironment",
        "//js:value",
    ],
    suppress = [
        "JSC_PRIMITIVE_OBJECT",
        "JSC_MISSING_RETURN_JSDOC",
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

ts_library(
    name = "ienvironment_ts",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "ienvironment.ts",
    ],
    deps = [
        "//js:value_ts",
    ],
)

closure_ts_library(
    name = "ienvironment",
    deps = [
        ":ienvironment_ts",
    ],
)