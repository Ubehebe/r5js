load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = ["//js:__subpackages__"],
)

ts_library(
    name = "io",
    srcs = glob(["*.ts"]),
    deps = [
        "//js/ast:datum",
        "//js/read",
        "//js/scan",
        "//js:value",
    ],
    tsconfig = "//:tsconfig.json",
)
