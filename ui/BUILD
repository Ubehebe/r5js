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

genrule(
    name = "index",
    srcs = [
        "//ui/xsl:index_desktop",
    ],
    outs = [
        "index.html",
    ],
    cmd = "cp $(<) $(@)",
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

http_server(
    name = "devserver",
    data = [
        "index.html",
        "r5rs.css",
        "r5rs.xhtml",
        ":rollup",
    ],
)
