load("@npm_bazel_typescript//:defs.bzl", "ts_library")

filegroup(
    name = "ui",
    srcs = [
        "index.html",
        "repl.css",
    ],
    visibility = [
        "//:__pkg__",
    ],
)

exports_files(["index.html"])

ts_library(
    name = "mockterm",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//:__pkg__",
    ],
)
