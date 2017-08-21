load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

# TODO turn into closure_js_library
filegroup(
    name = "parse",
    srcs = glob(["*.js"], exclude = [
        "nonterminal.js",
        "terminals.js",
    ]),
    visibility = ["//js:__pkg__"],
)

closure_js_library(
    name = "nonterminal",
    srcs = [
        "nonterminal.js",
    ],
    deps = [],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "terminal",
    srcs = [
        "terminals.js",
    ],
    deps = [],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)


