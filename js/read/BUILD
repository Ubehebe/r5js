load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "read",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    deps = [
        "//js:error",
        "//js/ast:datum",
        "//js/parse:nonterminals",
        "//js/parse:terminals",
        "//js/scan",
    ],
)
