load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "scan",
    srcs = [
        "token_stream.ts",
    ],
    deps = [
        "//js:error",
        "//js/ast:datum",
        "//js/parse:terminals",
    ],
    visibility = ["//js:__subpackages__"],
    tsconfig = "//:tsconfig.json",
)
