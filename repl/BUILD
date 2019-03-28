load("@build_bazel_rules_nodejs//:defs.bzl", "nodejs_binary")
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

nodejs_binary(
    name = "repl_bin",
    data = [
        "//platform/node",
    ],
    entry_point = "r5js/platform/node/repl.js",
)
