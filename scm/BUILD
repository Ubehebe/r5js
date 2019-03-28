load("//:rules/scheme_source.bzl", "scheme_source")
load("@npm_bazel_typescript//:defs.bzl", "ts_library")

scheme_source(
    name = "SYNTAX",
    src = "r5rs-syntax.scm",
)

scheme_source(
    name = "PROCEDURES",
    src = "r5rs-procedures.scm",
)

scheme_source(
    name = "TEST_FRAMEWORK",
    src = "unit-test.scm",
)

scheme_source(
    name = "TEST_FRAMEWORK_TESTS",
    src = "unit-test-tests.scm",
)

scheme_source(
    name = "R5RS_TESTS",
    src = "r5rs-tests.scm",
)

scheme_source(
    name = "NEGATIVE_TESTS",
    src = "negative-tests.scm",
)

scheme_source(
    name = "OTHER_TESTS",
    src = "other-tests.scm",
)

ts_library(
    name = "scm",
    srcs = [
        "scheme_sources.ts",
        ":PROCEDURES",
        ":SYNTAX",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//visibility:public",
    ],
)

ts_library(
    name = "test_sources",
    srcs = [
        "test_sources.ts",
        ":NEGATIVE_TESTS",
        ":OTHER_TESTS",
        ":R5RS_TESTS",
        ":TEST_FRAMEWORK",
        ":TEST_FRAMEWORK_TESTS",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//test:__pkg__",
    ],
)
