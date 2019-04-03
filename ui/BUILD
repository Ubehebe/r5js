load("@npm_bazel_typescript//:defs.bzl", "ts_library")
load("//:rules/embed_template_literal.bzl", "embed_template_literal")

exports_files(["r5rs.css"])

embed_template_literal(
    name = "banner",
    src = "banner.txt",
)

# If the banner was only used in :ui below, it could be a src of that target.
# But it is also used by //node, and ts_library forbids listing srcs from other packages. So make it
# its own library.
ts_library(
    name = "banner_lib",
    srcs = [
        ":banner",
    ],
    visibility = [
        "//node:__pkg__",
    ],
)

ts_library(
    name = "ui",
    srcs = glob(["*.ts"]),
    tsconfig = "//:tsconfig.json",
    visibility = [
        "//:__pkg__",
    ],
    deps = [
        ":banner_lib",
        "//eval:boot",
        "//repl",
        "//scm",
    ],
)
