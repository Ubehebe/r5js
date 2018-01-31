load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "macro",
    srcs = glob(["*.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js/ast",
        "//js:error",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
        "//js:value",
    ],
    visibility = ["//js:__subpackages__"],
    suppress = [
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)
