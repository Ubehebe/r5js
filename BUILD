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

genrule(
    name = "spec_xhtml",
    srcs = ["@spec//:r5rs.html"],
    outs = ["r5rs.xhtml"],
    # The spec takes a long time to parse, delaying the load and DOMContentLoaded events, so we
    # bring it in via ajax instead. Unfortunately, that means it has to be valid XHTML. I've
    # carefully ensured spec/r5rs.html is valid HTML5 and (with the addition of the following line)
    # XHTML(5), but perhaps we should run xmllint during the build process.
    cmd = """echo '<?xml version="1.0" encoding="UTF-8"?>' > $(@)
cat $(<) >> $(@)""",
)

_COMMON_SRCS = [
    "r5rs.xhtml",
    "robots.txt",
    "@spec//:images",
    "//ui:css/r5rs.css",
]

pkg_tar(
    name = "website_desktop",
    srcs = [
        "//xsl:index_desktop",
    ] + _COMMON_SRCS,
    extension = "tar.gz",
    remap_paths = {
        "index_desktop.html": "index.html",
    },
)

pkg_tar(
    name = "website_mobile",
    srcs = [
        "//xsl:index_mobile",
    ] + _COMMON_SRCS,
    extension = "tar.gz",
    remap_paths = {
        "index_mobile.html": "index.html",
    },
)
