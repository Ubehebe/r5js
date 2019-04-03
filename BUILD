load("@build_bazel_rules_nodejs//:defs.bzl", "http_server", "rollup_bundle")
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

genrule(
    name = "gen_index",
    srcs = [
        "README.md",
    ],
    outs = [
        "index.html",
    ],
    cmd = "< $(<) $(location @npm//commonmark/bin:commonmark) > $(@)",
    tools = [
        "@npm//commonmark/bin:commonmark",
    ],
    visibility = [
        "//ui:__pkg__",
    ],
)

rollup_bundle(
    name = "rollup",
    entry_point = "ui/main",
    deps = [
        "//ui",
    ],
)

http_server(
    name = "devserver",
    data = [
        "index.html",
        ":rollup",
        "//ui:r5rs.css",
    ],
)
