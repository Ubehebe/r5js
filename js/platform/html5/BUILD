load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "html5",
    srcs = glob(["*.js"]),
    language = "ECMASCRIPT6_STRICT",
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "//js:bbom",
        "//js/platform",
        "//js/repl",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
)