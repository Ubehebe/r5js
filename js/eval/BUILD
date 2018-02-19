load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "eval",
    srcs = glob(["*.js"], exclude = ["async_evaluator.js"]),
    visibility = ["//js:__subpackages__"],
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js/ast",
        "//js:error",
        "//js/io",
        "//js/parse",
        "//js/parse:nonterminals",
        "//js/read",
        "//js/scan",
        "//js/runtime",
        "//js/runtime:ienvironment",
        "//js:value",
    ],
    suppress = [
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "impl",
    srcs = ["async_evaluator.js"],
    deps = [
        "@io_bazel_rules_closure//closure/library",
        ":eval",
        "//js:boot",
        "//js:scheme_sources",
        "//js/io",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    suppress = [
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)