load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_binary", "closure_js_library")

BIG_BALL_OF_MUD_SRCS = glob([
    "ast/*.js",
    "eval/*.js",
    "io/*.js",
    "macro/*.js",
    "parse/*.js",
    "read/*.js",
    "runtime/*.js",
    "scan/*.js",
    "error.js",
])

closure_js_library(
    name = "bbom",
    srcs = BIG_BALL_OF_MUD_SRCS,
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//src:__subpackages__",
    ],
    deps = [
        "@io_bazel_rules_closure//closure/library",
    ],
    suppress = [
        "JSC_MUST_BE_PRIVATE",
        "JSC_PRIMITIVE_OBJECT",
    ],
)

closure_js_library(
    name = "js",
    testonly = 1,
    srcs = glob(
        ["**/*.js"],
        exclude = BIG_BALL_OF_MUD_SRCS + glob(["tdd/**/*.js"]),
    ),
    language = "ECMASCRIPT6_STRICT",
    deps = [
        ":bbom",
        "//src/js/repl",
#        "//src/js/platform/html5",
#        "//src/js/test",
#        "//src/js/test/matchers",
        "@io_bazel_rules_closure//closure/library",
        "@closure_tdd//:closure-tdd",
    ],
    visibility = [
        "//visibility:public",
    ],
)

closure_js_binary(
    name = "bin",
    testonly = 1,
    deps = [
        ":js",
    ],
)
