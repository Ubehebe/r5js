load("@build_bazel_rules_nodejs//:defs.bzl", "http_server", "rollup_bundle")
load("@com_github_bazelbuild_buildtools//buildifier:def.bzl", "buildifier")
load("//:rules/tslint.bzl", "tslint")
load("@bazel_pandoc//:pandoc.bzl", "pandoc")

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
    name = "pandoc_inputs",
    srcs = [
        # README.md must be listed first due to the way the pandoc rule is implemented. See
        # https://github.com/ProdriveTechnologies/bazel-pandoc/blob/b69f8591d55f1a80bf22df0009f4f959e57511e1/pandoc.bzl#L13.
        "README.md",
        "//ui:template.html",
    ],
)

# Run pandoc on README.md, producing index.html.
pandoc(
    name = "index",
    src = ":pandoc_inputs",
    from_format = "commonmark",
    # Surround the generated HTML with content from ui/template.html.
    # The filegroup is required as a hack to make ui/template.html available to the action.
    options = [
        "--standalone",
        "--template=ui/template.html",
    ],
    to_format = "html",
)

# TODO: this makes local development slow. Every time a source file changes, the whole bundle has to
# be rebuilt. Ideally, we could serve the sources unbundled, so that each source file change
# would only require recompiling its ts_library and any downstream deps. The rules_typescript
# ts_devserver rule does something like this. However, as of 2019, bundling is still the way to go
# in production. There is value in having our devserver be very similar to the production setup,
# even if it is a bit slow.
rollup_bundle(
    name = "rollup",
    entry_point = "ui/main",
    deps = [
        "//ui",
    ],
)

filegroup(
    name = "website",
    srcs = [
        ":index",
        ":rollup",
        "//ui:r5rs.css",
    ],
)

http_server(
    name = "devserver",
    data = [
        ":website",
    ],
)
