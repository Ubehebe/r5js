load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "scan",
    srcs = [
        "token_stream.ts",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = ["//js:__subpackages__"],
    deps = [
        "//js:error",
        "//js/ast:datum",
        "//js/parse:terminals",
    ],
)
