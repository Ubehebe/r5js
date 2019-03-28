load("@build_bazel_rules_nodejs//:defs.bzl", "nodejs_binary")
load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "node",
    srcs = [
        "repl.ts",
        "terminal.ts",
    ],
    tsconfig = "//:tsconfig.json",
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

nodejs_binary(
    name = "repl",
    data = [
        ":node",
    ],
    entry_point = "r5js/node/repl.js",
)
