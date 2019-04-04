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

pandoc(
    name = "index",
    src = "README.md",
    from_format = "commonmark",
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

http_server(
    name = "devserver",
    data = [
        "index.html",
        ":rollup",
        "//ui:r5rs.css",
    ],
)
