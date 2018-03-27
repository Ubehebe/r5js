load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "ts_web_test")

ts_library(
    name = "js_interop_test_lib",
    srcs = [
        "js_interop_test.ts",
        "custom_matchers.d.ts",
    ],
    deps = [
        "//js/ast:datum_ts",
        "//js/ast:type_ts",
        "//js/eval:eval_ts",
        "//js/io:io_ts",
        "//js/parse:terminals_ts",
        "//js:error_ts",
        "//js:scheme_sources_ts",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_web_test(
    name = "js_interop_test",
    deps = [
        ":js_interop_test_lib",
    ],
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
    tsconfig = "//:tsconfig.json",
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

ts_library(
    name = "scheme_test_lib",
    srcs = [
        "scheme_test.ts",
        "test_framework.ts",
    ],
    deps = [
        ":scheme_sources_ts",
        "//js/eval:async_evaluator_ts",
        "//js/eval:eval_ts",
        "//js/io:io_ts",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_web_test(
    name = "scheme_test",
    deps = [
        ":scheme_test_lib",
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