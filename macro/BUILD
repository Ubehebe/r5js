load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = ["//:__subpackages__"],
)

ts_library(
    name = "macro",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    deps = [
        "//:error",
        "//:value",
        "//ast:datum",
        "//parse:nonterminals",
        "//parse:parser",
        "//parse:rename_util",
        "//runtime:environment",
    ],
)
