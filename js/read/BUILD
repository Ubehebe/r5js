load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "rule_ts",
    srcs = ["rule.ts"],
    deps = [
        "//js/ast:datum_ts",
        "//js/scan:scan_ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "rule",
    deps = [
        ":rule_ts",
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
        ":grammar",
        ":reader",
        ":rule",
        ":rule_factory",
    ],
)

closure_js_library(
    name = "grammar",
    srcs = [
        "grammar.js",
    ],
    deps = [
        ":rule",
    ],
)

closure_js_library(
    name = "reader",
    srcs = [
        "reader.js",
    ],
    deps = [
        "//js/ast",
        "//js:error",
        ":grammar",
        ":rule",
        ":rule_factory",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "rule_factory",
    srcs = [
        "rule_factory.js",
    ],
    deps = [
        "//js/parse:terminals",
        ":grammar",
        ":rule",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)
