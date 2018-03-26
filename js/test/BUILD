load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library", "closure_js_test")
load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library", "ts_web_test")

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
        "//js:error",
        "//js/eval:shim",
        "//js:scheme_sources",
        "//js/io",
        "//js/parse:terminals",
        "//js/test/matchers",
    ],
    suppress = [
        "JSC_INEXISTENT_PROPERTY",
        "JSC_MISSING_RETURN_JSDOC",
        "JSC_UNKNOWN_EXPR_TYPE",
        "JSC_TYPE_MISMATCH",
    ]
)

ts_library(
    name = "parser_test_lib",
    srcs = [
        "parser_test.ts",
        "custom_matchers.d.ts",
    ],
    deps = [
        "//js/ast:datum_ts",
        "//js/eval:eval_ts",
        "//js/parse:nonterminals_ts",
        "//js/scan:scan_ts",
    ],
)

ts_web_test(
    name = "parser_test",
    deps = [
        ":parser_test_lib",
    ],
)

ts_library(
    name = "scanner_test_lib",
    srcs = [
        "scanner_test.ts",
        "custom_matchers.d.ts",
    ],
    deps = [
    "//js/ast:datum_ts",
    "//js/scan:scan_ts",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_web_test(
    name = "scanner_test",
    deps = [
        ":scanner_test_lib",
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
        "JSC_TYPE_MISMATCH",
        "JSC_UNKNOWN_EXPR_TYPE",
     ],
)

ts_library(
    name = "scheme_sources_ts",
    srcs = [
        "scheme_sources.ts",
    ],
    deps = [
        "//scm:NEGATIVE_TESTS",
        "//scm:OTHER_TESTS",
        "//scm:R5RS_TESTS",
        "//scm:TEST_FRAMEWORK",
        "//scm:TEST_FRAMEWORK_TESTS",
    ],
)

closure_ts_library(
    name = "scheme_sources",
    deps = [
        ":scheme_sources_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
    ],
)

closure_js_library(
    name = "lib",
    testonly = 1,
    srcs = [
        "test_framework.js",
    ],
    deps = [
        "@closure_tdd//:closure-tdd",
        "@io_bazel_rules_closure//closure/library",
        "//js/eval:shim",
        "//js/eval:impl",
        "//js/io",
        ":scheme_sources",
    ],
    suppress = [
            "JSC_TYPE_MISMATCH",
            "JSC_UNKNOWN_EXPR_TYPE",
    ]
)