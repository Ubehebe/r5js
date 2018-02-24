load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
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
        "//js/scan",
        ":grammar",
        ":rule",
        ":rule_factory",
    ]
)

closure_js_library(
    name = "rule",
    srcs = [
        "rule.js",
    ],
    deps = [
        "//js/ast",
        "//js/scan",
    ]
)

closure_js_library(
    name = "rule_factory",
    srcs = [
        "rule_factory.js",
    ],
    deps = [
        "//js/parse:terminals",
        "//js/scan",
        ":grammar",
        ":rule",
    ],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)
