load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "node",
    srcs = [
        "terminal.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js/repl",
        "@npm//@types/node",
    ],
)
