load("@build_bazel_rules_nodejs//:defs.bzl", "rollup_bundle")
load("@npm_bazel_typescript//:defs.bzl", "ts_library")

exports_files([
    "index.html",
    "css/r5rs.css",
])

ts_library(
    name = "ui",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    deps = [
        "//eval:boot",
        "//repl",
        "//scm",
        "//tutorial",
    ],
)

rollup_bundle(
    name = "rollup",
    entry_point = "ui/main",
    deps = [
        ":ui",
    ],
)
