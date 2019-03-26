load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = ["//js:__subpackages__"],
)

ts_library(
    name = "macro",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js:error",
        "//js:value",
        "//js/ast:datum",
        "//js/parse:nonterminals",
        "//js/parse:parser",
        "//js/parse:rename_util",
        "//js/runtime:environment",
    ],
)
