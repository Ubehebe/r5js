load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

SRCS_USED_BY_PARSER_IMPL = [
    "assignment.js",
    "branch.js",
    "continuation.js",
    "errors.js",
    "lambda.js",
    "procedure.js",
    "proc_call.js",
    "top_level_assignment.js",
    "top_level_syntax_assignment.js",
#    these depend on parse/util.js
#    "user_defined_procedure.js",
#    "varargs_user_defined_procedure.js",
]

closure_js_library(
    name = "used_by_parser_impl",
    srcs = SRCS_USED_BY_PARSER_IMPL,
    deps = [
        "//js/ast",
        "//js:error",
        "//js/macro",
        "//js/parse:nonterminals",
        "//js/parse:parser",
        "//js/parse:terminals",
        "//js:value",
        ":ienvironment",
        ":proc_call_like",
    ],
    suppress = [
        "JSC_MUST_BE_PRIVATE",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = ["//js:__subpackages__"],
)

filegroup(
    name = "runtime",
    srcs = glob(["*.js"], exclude = [
        "ienvironment.js",
        "proc_call_like.js",
    ] + SRCS_USED_BY_PARSER_IMPL),
    visibility = ["//js:__pkg__"],
)

closure_js_library(
    name = "ienvironment",
    srcs = [
        "ienvironment.js",
    ],
    deps = [
        "//js:value",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "proc_call_like",
    srcs = [
        "proc_call_like.js",
    ],
    deps = [
        ":ienvironment",
        "//js:value",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)