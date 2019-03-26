load("@npm_bazel_typescript//:defs.bzl", "ts_library")

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
        "//js/eval",
        "//js/io",
        "//js/parse:parser_impl",
        "//js/read",
        "//js/runtime",
        "//js/runtime:environment",
        "//js/runtime:primitive_procedures",
        "//js/runtime:trampoline",
        "//js/scan",
    ],
)
