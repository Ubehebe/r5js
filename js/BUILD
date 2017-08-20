load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

BIG_BALL_OF_MUD_SRCS = glob([
    "ast/*.js",
    "eval/**/*.js",
    "io/*.js",
    "macro/*.js",
    "parse/*.js",
    "read/*.js",
    "runtime/*.js",
    "scan/*.js",
    "boot.js",
])

closure_js_library(
    name = "value",
    srcs = [
        "value.js",
    ],
    language = "ECMASCRIPT6_STRICT",
    deps = [],
    visibility = [
        "//js:__subpackages__",
    ],
)

closure_js_library(
    name = "error",
    srcs = [
        "error.js",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
)

# TODO: break deps cycles
closure_js_library(
    name = "bbom",
    srcs = BIG_BALL_OF_MUD_SRCS,
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ],
    deps = [
        "@io_bazel_rules_closure//closure/library",
        ":error",
        ":value",
    ],
    suppress = [
        "JSC_MUST_BE_PRIVATE",
        "JSC_PRIMITIVE_OBJECT",
    ],
)
