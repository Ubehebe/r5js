load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")
load("//:builddefs.bzl", "JS_VERSION")

closure_js_library(
    name = "macro",
    srcs = glob(["*.js"]),
    deps = [
        "//js/ast",
        "//js:error",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
        "//js/runtime:ienvironment",
        "//js/runtime:proc_call_like",
        "//js:value",
    ],
    language = JS_VERSION,
    visibility = ["//js:__subpackages__"],
)
