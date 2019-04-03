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
