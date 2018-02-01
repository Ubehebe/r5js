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