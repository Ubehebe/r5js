load("//:builddefs.bzl", "JS_VERSION")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library", "closure_js_test")

closure_js_test(
    name = "js_interop_test",
    pedantic = True,
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
        "//js:scheme_sources",
        "//js/io",
        "//js/parse:terminals",
        "//js/runtime:used_by_parser_impl",
        "//js/test/matchers",
    ],
)

closure_js_test(
    name = "parser_test",
    pedantic = True,
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
    pedantic = True,
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

closure_js_test(
    name = "scheme_test",
    pedantic = True,
    language = JS_VERSION,
    srcs = [
        "scheme_test.js",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library",
        "@io_bazel_rules_closure//closure/library:testing",
        ":lib",
        "//js/io",
        "//js/platform/common",
    ],
    entry_points = [
        "goog:r5js.test.SchemeTest",
    ],
)

closure_js_library(
    name = "lib",
    language = JS_VERSION,
    testonly = 1,
    srcs = [
        "scheme_sources.js",
        "test_framework.js",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library",
        "//js/io",
        "//js/platform/common",
        "//scm:NEGATIVE_TESTS",
        "//scm:OTHER_TESTS",
        "//scm:R5RS_TESTS",
        "//scm:TEST_FRAMEWORK",
        "//scm:TEST_FRAMEWORK_TESTS",
    ],
)