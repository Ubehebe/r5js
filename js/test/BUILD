load("//:builddefs.bzl", "node_test")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_binary", "closure_js_library", "closure_js_test")

closure_js_library(
    name = "lib",
    testonly = 1,
    srcs = glob(
        ["*.js"],
        exclude = [
            "js_interop_test.js",
            "parser_test.js",
            "scanner_test.js",
        ]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "@io_bazel_rules_closure//closure/library:testing",
        "@closure_tdd//:closure-tdd",
        "//js:bbom",
        "//js:error",
        "//js/platform/common",
        "//js/platform/node",
        "//js/test/matchers",
        "//scm:TEST_FRAMEWORK",
        "//scm:TEST_FRAMEWORK_TESTS",
        "//scm:R5RS_TESTS",
        "//scm:NEGATIVE_TESTS",
        "//scm:OTHER_TESTS",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//js:__subpackages__",
    ]
)

closure_js_binary(
    name = "bin",
    testonly = 1,
    debug = True,
    deps = [
        ":lib",
    ],
    compilation_level = "simple",
    entry_points = [
        "goog:r5js.test.main",
    ],
)

closure_js_test(
    name = "js_interop_test",
    language = "ECMASCRIPT6_STRICT",
    srcs = [
        "js_interop_test.js",
    ],
    entry_points = [
        "goog:r5js.test.JsInterop",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library:testing",
        "//js:bbom",
        "//js:error",
        "//js/parse:terminal",
        "//js/platform",
        "//js/test/matchers",
    ],
)

closure_js_test(
    name = "parser_test",
    language = "ECMASCRIPT6_STRICT",
    srcs = [
        "parser_test.js",
    ],
    entry_points = [
        "goog:r5js.test.Parser",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library:testing",
        "//js:bbom",
        "//js:error",
        "//js/parse:nonterminal",
        "//js/test/matchers",
    ],
)

closure_js_test(
    name = "scanner_test",
    language = "ECMASCRIPT6_STRICT",
    srcs = [
        "scanner_test.js",
    ],
    entry_points = [
        "goog:r5js.test.Scanner",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library:testing",
        "//js:bbom",
        "//js:error",
        "//js/test/matchers",
    ],
)

node_test(
    name = "test",
#    debug = True,
    src = ":bin.js",
    entry_point = "r5js.test.main",
)