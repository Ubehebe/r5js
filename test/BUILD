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
        "//ast:datum",
        "//ast:type",
        "//base:error",
        "//eval",
        "//eval:boot",
        "//io",
        "//parse:terminals",
        "//runtime",
        "//scm",
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
        "//ast:datum",
        "//eval",
        "//parse:nonterminals",
        "//parse:parser_impl",
        "//read",
        "//scan",
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
        "//ast:datum",
        "//scan",
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
        "//eval",
        "//eval:async_evaluator",
        "//io",
        "//scm:test_sources",
        "@npm//@types/jasmine",
    ],
)

jasmine_node_test(
    name = "scheme_test",
    deps = [
        ":scheme_test_lib",
    ],
)
