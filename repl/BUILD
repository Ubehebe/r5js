load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "repl",
    srcs = [
        "repl.ts",
        "replutil.ts",
        "terminal.ts",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//visibility:public",
    ],
    deps = [
        "//eval",
        "//runtime",
    ],
)
