load("//:builddefs.bzl", "JS_VERSION", "node_test")
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
        "//js:boot",
        "//js:error",
        "//js/eval",
        "//js/io",
        "//js/platform/common",
        "//js/platform/node",
        "//js/test/matchers",
        "//scm:TEST_FRAMEWORK",
        "//scm:TEST_FRAMEWORK_TESTS",
        "//scm:R5RS_TESTS",
        "//scm:NEGATIVE_TESTS",
        "//scm:OTHER_TESTS",
    ],
    language = JS_VERSION,
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
    language = JS_VERSION,
    srcs = [
        "js_interop_test.js",
    ],
    entry_points = [
        "goog:r5js.test.JsInterop",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library:testing",
        "//js/ast",
        "//js:boot",
        "//js:error",
        "//js/io",
        "//js/parse:terminals",
        "//js/platform",
        "//js/runtime:used_by_parser_impl",
        "//js/test/matchers",
    ],
)

closure_js_test(
    name = "parser_test",
    language = JS_VERSION,
    srcs = [
        "parser_test.js",
    ],
    entry_points = [
        "goog:r5js.test.Parser",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library:testing",
        "//js:boot",
        "//js:error",
        "//js/parse:nonterminals",
        "//js/test/matchers",
    ],
)

closure_js_test(
    name = "scanner_test",
    language = JS_VERSION,
    srcs = [
        "scanner_test.js",
    ],
    entry_points = [
        "goog:r5js.test.Scanner",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library:testing",
        "//js/ast",
        "//js:boot",
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