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
        "//:boot",
        "//:scheme_sources",
        "//eval",
        "//io",
        "//repl",
        "//runtime",
        "@npm//@types/node",
    ],
)
