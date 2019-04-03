load("//:rules/embed_template_literal.bzl", "embed_template_literal")
load("@npm_bazel_typescript//:defs.bzl", "ts_library")

embed_template_literal(
    name = "SYNTAX",
    src = "r5rs-syntax.scm",
)

embed_template_literal(
    name = "PROCEDURES",
    src = "r5rs-procedures.scm",
)

embed_template_literal(
    name = "TEST_FRAMEWORK",
    src = "unit-test.scm",
)

embed_template_literal(
    name = "TEST_FRAMEWORK_TESTS",
    src = "unit-test-tests.scm",
)

embed_template_literal(
    name = "R5RS_TESTS",
    src = "r5rs-tests.scm",
)

embed_template_literal(
    name = "NEGATIVE_TESTS",
    src = "negative-tests.scm",
)

embed_template_literal(
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
