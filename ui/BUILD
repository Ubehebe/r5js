load("@npm_bazel_typescript//:defs.bzl", "ts_library")
load("//:rules/embed_template_literal.bzl", "embed_template_literal")

exports_files(["r5rs.css"])

genrule(
    name = "banner_stamped",
    srcs = [
        "banner.txt",
    ],
    outs = [
        "banner_stamped.txt",
    ],
    cmd = """cp $(<) $(@)
echo "\n;; built at $$(grep ^STABLE_HEAD bazel-out/stable-status.txt | cut -c 13-19)" >> $(@)""",
    stamp = True,  # provides access to bazel-out/stable-status.txt
)

embed_template_literal(
    name = "banner",
    src = "banner_stamped",
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
