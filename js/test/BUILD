load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library", "closure_js_test")

closure_js_test(
    name = "js_interop_test",
    srcs = [
        "js_interop_test.js",
    ],
    entry_points = [
        "goog:r5js.test.JsInterop",
    ],
    defs = [
        "--process_common_js_modules",
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
    suppress = [
        "JSC_MISSING_RETURN_JSDOC",
        "JSC_UNKNOWN_EXPR_TYPE",
        "JSC_TYPE_MISMATCH",
    ]
)

closure_js_test(
    name = "parser_test",
    srcs = [
        "parser_test.js",
    ],
        defs = [
            "--process_common_js_modules",
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
    suppress = [
            "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_test(
    name = "scanner_test",
    srcs = [
        "scanner_test.js",
    ],
        defs = [
            "--process_common_js_modules",
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
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
     ],
)

closure_js_test(
    name = "scheme_test",
    srcs = [
        "scheme_test.js",
    ],
        defs = [
            "--process_common_js_modules",
        ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library",
        "@io_bazel_rules_closure//closure/library:testing",
        ":lib",
        "//js/eval:impl",
        "//js/io",
    ],
    entry_points = [
        "goog:r5js.test.SchemeTest",
    ],
    suppress = [
        "JSC_IMPLICITLY_NULLABLE_JSDOC",
        "JSC_MISSING_JSDOC",
        "JSC_UNKNOWN_EXPR_TYPE",
     ],
)

closure_js_library(
    name = "lib",
    testonly = 1,
    srcs = [
        "scheme_sources.js",
        "test_framework.js",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library",
        "//js/eval",
        "//js/eval:impl",
        "//js/io",
        "//scm:NEGATIVE_TESTS",
        "//scm:OTHER_TESTS",
        "//scm:R5RS_TESTS",
        "//scm:TEST_FRAMEWORK",
        "//scm:TEST_FRAMEWORK_TESTS",
    ],
    suppress = [
            "JSC_INCORRECT_SHORTNAME_CAPITALIZATION",
            "JSC_UNKNOWN_EXPR_TYPE",
    ]
)