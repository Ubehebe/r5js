load("@npm_bazel_jasmine//:index.bzl", "jasmine_node_test")
load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "js_interop_test_lib",
    srcs = [
        "custom_matchers.d.ts",
        "js_interop_test.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js:boot",
        "//js:error",
        "//js:scheme_sources",
        "//js/ast:datum",
        "//js/ast:type",
        "//js/eval",
        "//js/io",
        "//js/parse:terminals",
        "//js/runtime",
        "@npm//@types/jasmine",
    ],
)

jasmine_node_test(
    name = "js_interop_test",
    deps = [
        ":js_interop_test_lib",
    ],
)

ts_library(
    name = "parser_test_lib",
    srcs = [
        "custom_matchers.d.ts",
        "parser_test.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js/ast:datum",
        "//js/eval",
        "//js/parse:nonterminals",
        "//js/parse:parser_impl",
        "//js/read",
        "//js/scan",
        "@npm//@types/jasmine",
    ],
)

jasmine_node_test(
    name = "parser_test",
    deps = [
        ":parser_test_lib",
    ],
)

ts_library(
    name = "scanner_test_lib",
    srcs = [
        "custom_matchers.d.ts",
        "scanner_test.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js/ast:datum",
        "//js/scan",
        "@npm//@types/jasmine",
    ],
)

jasmine_node_test(
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
    tsconfig = "//:tsconfig.json",
    deps = [
        ":scheme_sources",
        "//js/eval",
        "//js/eval:async_evaluator",
        "//js/io",
        "@npm//@types/jasmine",
    ],
)

jasmine_node_test(
    name = "scheme_test",
    deps = [
        ":scheme_test_lib",
    ],
)

ts_library(
    name = "scheme_sources",
    srcs = [
        "scheme_sources.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//scm:NEGATIVE_TESTS",
        "//scm:OTHER_TESTS",
        "//scm:R5RS_TESTS",
        "//scm:TEST_FRAMEWORK",
        "//scm:TEST_FRAMEWORK_TESTS",
    ],
)
