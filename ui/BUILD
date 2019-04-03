load("@npm_bazel_typescript//:defs.bzl", "ts_library")

exports_files(["r5rs.css"])

ts_library(
    name = "ui",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//:__pkg__",
    ],
    deps = [
        "//eval:boot",
        "//repl",
        "//scm",
    ],
)
