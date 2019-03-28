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
        "//eval",
        "//eval:boot",
        "//io",
        "//repl",
        "//runtime",
        "//scm",
        "@npm//@types/node",
    ],
)
