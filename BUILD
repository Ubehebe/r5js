load("@build_bazel_rules_nodejs//:defs.bzl", "nodejs_binary")
load("@com_github_bazelbuild_buildtools//buildifier:def.bzl", "buildifier")
load("//:rules/tslint.bzl", "tslint")

exports_files(["tsconfig.json"])

buildifier(
    name = "buildifier",
    lint_mode = "fix",
    mode = "fix",
)

buildifier(
    name = "buildifier_pre_commit",
    mode = "check",
)

tslint(
    name = "tslint",
    config = "tslint.json",
)

filegroup(
    name = "web_repl",
    srcs = [
        "//ui",
        "@mockterm",
    ],
)

nodejs_binary(
    name = "repl",
    data = [
        "//js/platform/node",
    ],
    entry_point = "r5js/js/platform/node/repl.js",
)
