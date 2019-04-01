load("@npm_bazel_typescript//:defs.bzl", "ts_library")

exports_files(["index.html"])

ts_library(
    name = "ui",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//:__pkg__",
    ],
    deps = [
        "@r5js//ui:mockterm",
    ],
)
