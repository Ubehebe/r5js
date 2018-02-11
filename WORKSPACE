git_repository(
    name = "io_bazel_rules_closure",
    commit = "08039ba8ca59f64248bb3b6ae016460fe9c9914f",
    remote = "https://github.com/bazelbuild/rules_closure.git",
)

git_repository(
    name = "closure_tdd",
    remote = "https://github.com/Ubehebe/closure-tdd.git",
    commit = "ae6e991f0684269145b0200f8188a8ea5db0116e",
)

load("@io_bazel_rules_closure//closure:defs.bzl", "closure_repositories")
closure_repositories()

git_repository(
    name = "build_bazel_rules_nodejs",
    commit = "ff1a9caecb6b11ff34f2d1975aafe6c812014c63",
    remote = "https://github.com/bazelbuild/rules_nodejs.git",
)

load("@build_bazel_rules_nodejs//:defs.bzl", "npm_install", "node_repositories")
node_repositories(package_json = ["@build_bazel_rules_typescript//:package.json"])

git_repository(
    name = "build_bazel_rules_typescript",
    commit = "dd4169d3e1d6b8f6eda1d7c1f29fb9368542f16a",
    remote = "https://github.com/Ubehebe/rules_typescript.git",
)

npm_install(
    name = "upstream_node_modules",
    package_json = "@build_bazel_rules_typescript//:package.json",
)

load("@build_bazel_rules_typescript//:defs.bzl", "ts_setup_workspace")
ts_setup_workspace()