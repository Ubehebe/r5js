load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

# TODO turn into closure_js_library
filegroup(
    name = "parse",
    srcs = glob(["*.js"], exclude = [
        "nonterminals.js",
        "parser.js",
        "rename_util.js",
        "terminals.js",
    ]),
    visibility = ["//js:__pkg__"],
)

closure_js_library(
    name = "nonterminals",
    srcs = [
        "nonterminals.js",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "parser",
    srcs = [
        "parser.js",
    ],
    deps = [
        "//js/ast",
        ":nonterminals",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "rename_util",
    srcs = [
        "rename_util.js",
    ],
    deps = [
        ":terminals",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "terminals",
    srcs = [
        "terminals.js",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)


