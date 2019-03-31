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

ts_library(
    name = "mockterm",
    srcs = glob(["*.ts"]),
    visibility = [
        "//:__pkg__",
    ],
)
