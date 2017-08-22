load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "scan",
    srcs = glob(["*.js"]),
    deps = [
        "//js:error",
        "//js/ast",
        "//js/parse:terminals",
    ],
    suppress = [
        "JSC_PRIMITIVE_OBJECT",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = ["//js:__subpackages__"],
)
