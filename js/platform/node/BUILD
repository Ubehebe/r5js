load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "node",
    srcs = [
        "repl.ts",
        "terminal.ts",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//visibility:public",
    ],
    deps = [
        "//js:boot",
        "//js:scheme_sources",
        "//js/eval",
        "//js/io",
        "//js/repl",
        "//js/runtime",
        "@npm//@types/node",
    ],
)
