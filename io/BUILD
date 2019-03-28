load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = ["//:__subpackages__"],
)

ts_library(
    name = "io",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    deps = [
        "//:value",
        "//ast:datum",
        "//read",
        "//scan",
    ],
)
