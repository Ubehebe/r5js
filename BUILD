load("@com_github_bazelbuild_buildtools//buildifier:def.bzl", "buildifier")
load("//:rules/tslint.bzl", "tslint")
load("@bazel_tools//tools/build_defs/pkg:pkg.bzl", "pkg_tar")

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

_COMMON_SRCS = [
    "@spec//:images",
]

pkg_tar(
    name = "website_desktop",
    srcs = [
        "//ui/xsl:index_desktop",
    ] + _COMMON_SRCS,
    extension = "tar.gz",
    remap_paths = {
        "index_desktop.html": "index.html",
    },
)

pkg_tar(
    name = "website_mobile",
    srcs = [
        "//ui/xsl:index_mobile",
    ] + _COMMON_SRCS,
    extension = "tar.gz",
    remap_paths = {
        "index_mobile.html": "index.html",
    },
)
