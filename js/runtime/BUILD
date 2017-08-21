load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

filegroup(
    name = "runtime",
    srcs = glob(["*.js"], exclude = [
        "ienvironment.js",
        "proc_call_like.js",
    ]),
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

closure_js_library(
    name = "proc_call_like",
    srcs = [
        "proc_call_like.js",
    ],
    deps = [
        ":ienvironment",
        "//js:value",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)