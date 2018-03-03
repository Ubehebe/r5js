load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

ts_library(
    name = "macro_ts",
    srcs = [
        "rename_helper.ts",
    ],
    deps = [
        "//js:error_ts",
        "//js/parse:rename_util_ts",
    ],
)

closure_ts_library(
    name = "shim",
    deps = [
        ":macro_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_UNKNOWN_EXPR_TYPE",
    ]
)

closure_js_library(
    name = "macro",
    srcs = glob(["*.js"]),
    deps = [
        ":shim",
        "@io_bazel_rules_closure//closure/library",
        "//js:error",
        "//js/parse:rename_util",
        "//js/parse:shim",
        "//js/runtime:ienvironment",
        "//js:value",
    ],
    visibility = ["//js:__subpackages__"],
    suppress = [
            "JSC_INVALID_CAST",
            "JSC_TYPE_MISMATCH",
            "JSC_UNDEFINED_VARIABLE",
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)
