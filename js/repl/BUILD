load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "repl",
    srcs = [
        "replutil.ts",
        "terminal.ts",
    ],
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js/eval",
        "//js/runtime",
    ],
)
