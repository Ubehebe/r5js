load("@com_github_bazelbuild_buildtools//buildifier:def.bzl", "buildifier")
load("//:rules/tslint.bzl", "tslint")

exports_files(["tsconfig.json"])

buildifier(
    name = "buildifier",
    lint_mode = "fix",
    mode = "fix",
)

buildifier(
    name = "buildifier_pre_commit",
    mode = "check",
)

tslint(
    name = "tslint",
    config = "tslint.json",
)

filegroup(
    name = "web_repl",
    srcs = [
        "//ui",
        "@mockterm",
    ],
)

load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//:__subpackages__",
    ],
)

ts_library(
    name = "scheme_sources",
    srcs = [
        "scheme_sources.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
)

ts_library(
    name = "value",
    srcs = [
        "value.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_library(
    name = "error",
    srcs = [
        "error.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_library(
    name = "boot",
    srcs = [
        "boot.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//eval",
        "//io",
        "//parse:parser_impl",
        "//read",
        "//runtime",
        "//runtime:environment",
        "//runtime:primitive_procedures",
        "//runtime:trampoline",
        "//scan",
    ],
)
