load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "io",
    srcs = glob(["*.js"]),
    deps = [
        "//js/ast",
        "//js/read",
        "//js/scan",
        "//js:value",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = ["//js:__subpackages__"],
)
