load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository", "new_git_repository")

# Needed to build buildifier.
git_repository(
    name = "io_bazel_rules_go",
    remote = "https://github.com/bazelbuild/rules_go.git",
    tag = "0.18.1",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_register_toolchains", "go_rules_dependencies")

go_rules_dependencies()

go_register_toolchains()

git_repository(
    name = "com_github_bazelbuild_buildtools",
    remote = "https://github.com/bazelbuild/buildtools.git",
    tag = "0.22.0",
)

load("@com_github_bazelbuild_buildtools//buildifier:deps.bzl", "buildifier_dependencies")

buildifier_dependencies()

git_repository(
    name = "build_bazel_rules_nodejs",
    remote = "https://github.com/bazelbuild/rules_nodejs.git",
    tag = "0.27.8",
)

load("@build_bazel_rules_nodejs//:defs.bzl", "yarn_install")

yarn_install(
    name = "npm",
    package_json = "//:package.json",
    yarn_lock = "//:yarn.lock",
)

load("@npm//:install_bazel_dependencies.bzl", "install_bazel_dependencies")

install_bazel_dependencies()

git_repository(
    name = "r5js",
    commit = "b7f0ca443452b1bac9c13d677fbe06d1c0e35f2c",
    remote = "https://github.com/Ubehebe/r5js.git",
)

new_git_repository(
    name = "spec",
    build_file = "//:BUILD.spec",
    commit = "b9fe77910e60519491097f66e7e7b1bcd2ce0764",
    remote = "https://github.com/Ubehebe/r5rs-html5.git",
)
