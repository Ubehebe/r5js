load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

# TODO turn into closure_js_library
filegroup(
    name = "eval",
    srcs = glob(["**/*.js"], exclude = ["valutil.js"]),
    visibility = ["//js:__pkg__"],
)

# TODO used by runtime and eval. Move to runtime?
closure_js_library(
    name = "valutil",
    srcs = [
        "valutil.js",
    ],
    visibility = [
        "//js:__subpackages__",
    ],
    language = "ECMASCRIPT6_STRICT",
    deps = [
        "//js/ast",
        "//js/io",
        "//js/parse:terminals",
        "//js/runtime:used_by_parser_impl",
        "//js:value",
    ],
)
