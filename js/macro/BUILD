load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = ["//js:__subpackages__"],
)

ts_library(
    name = "macro",
    srcs = glob(["*.ts"]),
    deps = [
        "//js/ast:datum",
        "//js:error",
        "//js:value",
        "//js/parse:nonterminals",
        "//js/parse:parser",
        "//js/parse:rename_util",
        "//js/runtime:environment",
    ],
)