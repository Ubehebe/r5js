load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "scheme_sources",
    srcs = [
        "scheme_sources.ts",
    ],
    deps = [
        "//scm:PROCEDURES",
        "//scm:SYNTAX",
    ],
)

ts_library(
    name = "value",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "value.ts",
    ],
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
    deps = [
        "//js/io",
        "//js/eval",
        "//js/parse:parser_impl",
        "//js/read",
        "//js/runtime",
        "//js/runtime:environment",
        "//js/runtime:primitive_procedures",
        "//js/runtime:trampoline",
        "//js/scan",
    ],
)