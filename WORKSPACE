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

http_archive(
    name = "build_bazel_rules_nodejs",
    url = "https://github.com/bazelbuild/rules_nodejs/archive/0.0.2.tar.gz",
    strip_prefix = "rules_nodejs-0.0.2",
    sha256 = "976e6760d3f90fa0124d9c59276f19da344333813018ba9d7475e68b708ee46f",
)

load("@io_bazel_rules_closure//closure:defs.bzl", "closure_repositories")
closure_repositories()

load("@build_bazel_rules_nodejs//:defs.bzl", "node_repositories")
node_repositories(package_json = ["//:package.json"])