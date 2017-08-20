load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

filegroup(
    name = "runtime",
    srcs = glob(["*.js"], exclude = ["ienvironment.js"]),
    visibility = ["//js:__pkg__"],
)

closure_js_library(
    name = "ienvironment",
    srcs = [
        "ienvironment.js",
    ],
    deps = [
        "//js:value",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)