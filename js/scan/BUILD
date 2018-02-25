load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

exports_files(["token_stream.ts"])

ts_library(
    name = "scan_ts",
    srcs = [
        "token_stream.ts",
    ],
    deps = [
        "//js:error_ts",
        "//js/ast:datum_ts",
        "//js/parse:terminals_ts",
    ],
    visibility = ["//js:__subpackages__"],
)
