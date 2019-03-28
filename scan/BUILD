load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "scan",
    srcs = [
        "token_stream.ts",
    ],
    tsconfig = "//:tsconfig.json",
    visibility = ["//:__subpackages__"],
    deps = [
        "//ast:datum",
        "//base:error",
        "//parse:terminals",
    ],
)
