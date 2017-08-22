load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "common",
    language = "ECMASCRIPT6_STRICT",
    srcs = glob(["*.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js:boot",
        "//js/eval",
        "//js/platform",
        "//js/io",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)