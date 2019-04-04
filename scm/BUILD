load("//:rules/embed_template_literal.bzl", "embed_template_literal")
load("@npm_bazel_typescript//:defs.bzl", "ts_library")

embed_template_literal(
    name = "r5rs",
    srcs = [
        "r5rs-procedures.scm",
        "r5rs-syntax.scm",
    ],
)

ts_library(
    name = "scm",
    srcs = [
        "scheme_sources.ts",
        ":r5rs",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//visibility:public",
    ],
)

embed_template_literal(
    name = "tests",
    srcs = [
        "negative-tests.scm",
        "other-tests.scm",
        "r5rs-tests.scm",
        "unit-test.scm",
        "unit-test-tests.scm",
    ],
)

ts_library(
    name = "test_sources",
    srcs = [
        "test_sources.ts",
        ":tests",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//test:__pkg__",
    ],
)
