load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "ast",
    srcs = glob(["*.js"]),
    deps = [
        "//js:error",
        "//js:value",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    suppress = [
        "JSC_PRIMITIVE_OBJECT",
    ],
    language = "ECMASCRIPT6_STRICT",
)
