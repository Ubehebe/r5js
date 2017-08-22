load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "platform",
    language = "ECMASCRIPT6_STRICT",
    srcs = glob(["*.js"]),
    deps = [
        "//js:boot",
        "//js/eval",
        "//js/io",
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)