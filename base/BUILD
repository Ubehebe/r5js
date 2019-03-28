load("@npm_bazel_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//:__subpackages__",
    ],
)

ts_library(
    name = "value",
    srcs = [
        "value.ts",
    ],
    tsconfig = "//:tsconfig.json",
)

ts_library(
    name = "error",
    srcs = [
        "error.ts",
    ],
    tsconfig = "//:tsconfig.json",
)
