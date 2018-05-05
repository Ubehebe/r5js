git_repository(
    name = "build_bazel_rules_nodejs",
    tag = "0.8.0",
    remote = "https://github.com/bazelbuild/rules_nodejs.git",
)

load("@build_bazel_rules_nodejs//:defs.bzl", "npm_install", "node_repositories")
node_repositories(package_json = ["@build_bazel_rules_typescript//:package.json"])

git_repository(
    name = "build_bazel_rules_typescript",
    tag = "0.12.3",
    remote = "https://github.com/bazelbuild/rules_typescript.git",
)

npm_install(
    name = "upstream_node_modules",
    package_json = "@build_bazel_rules_typescript//:package.json",
)

load("@build_bazel_rules_typescript//:defs.bzl", "ts_setup_workspace")
ts_setup_workspace()