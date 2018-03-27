load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

exports_files(["boot.ts"])

ts_library(
    name = "scheme_sources_ts",
    srcs = [
        "scheme_sources.ts",
    ],
    deps = [
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
)

ts_library(
    name = "value_ts",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "value.ts",
    ],
)

ts_library(
    name = "error_ts",
    srcs = [
        "error.ts",
    ],
    tsconfig = "//:tsconfig.json",
)