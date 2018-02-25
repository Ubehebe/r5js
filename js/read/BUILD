load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "read_ts",
    srcs = glob(["*.ts"]),
    deps = [
        "//js/ast:datum_ts",
        "//js/parse:nonterminals_ts",
        "//js/parse:terminals_ts",
        "//js/scan:scan_ts",
        "//js:error_ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "shim",
    deps = [
        ":read_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "read",
    srcs = [],
    exports = [
        ":shim",
    ],
)