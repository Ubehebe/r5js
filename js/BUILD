load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "scheme_sources",
    srcs = [
        "scheme_sources.js",
    ],
    suppress = [
        "JSC_INCORRECT_SHORTNAME_CAPITALIZATION",
    ],
    deps = [
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
)

ts_library(
    name = "value_ts",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "value.ts",
    ],
)

closure_ts_library(
    name = "value",
    deps = [
        ":value_ts",
    ],
)

ts_library(
    name = "error_ts",
    srcs = [
        "error.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "error",
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":error_ts",
    ],
)

closure_js_library(
    name = "boot",
    srcs = ["boot.js"],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    deps = [
        ":error",
        ":value",
        "//js/eval",
        "//js/runtime:shim",
        "//js/parse:parser_impl",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/runtime",
        "//js/runtime:ienvironment",
    ],
)
