load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

exports_files(["token_stream.ts"])

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
)
