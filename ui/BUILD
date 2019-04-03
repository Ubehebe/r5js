load("@build_bazel_rules_nodejs//:defs.bzl", "http_server", "rollup_bundle")
load("@npm_bazel_typescript//:defs.bzl", "ts_library")

ts_library(
    name = "ui",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    deps = [
        "//eval:boot",
        "//repl",
        "//scm",
    ],
)

rollup_bundle(
    name = "rollup",
    entry_point = "ui/main",
    deps = [
        ":ui",
    ],
)

genrule(
    name = "index",
    srcs = [
        "//:index.html",
    ],
    outs = [
        "index.html",
    ],
    cmd = "cp $(<) $(@)",
)

http_server(
    name = "devserver",
    data = [
        "index.html",
        "r5rs.css",
        ":rollup",
    ],
)
