load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

exports_files(["boot.ts"])

ts_library(
    name = "scheme_sources_ts",
    srcs = [
        "scheme_sources.ts",
    ],
    deps = [
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
)

closure_ts_library(
    name = "scheme_sources",
    deps = [
        ":scheme_sources_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
    ]
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