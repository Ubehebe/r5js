http_archive(
    name = "io_bazel_rules_closure",
    strip_prefix = "rules_closure-0.4.1",
    sha256 = "ba5e2e10cdc4027702f96e9bdc536c6595decafa94847d08ae28c6cb48225124",
    url = "http://mirror.bazel.build/github.com/bazelbuild/rules_closure/archive/0.4.1.tar.gz",
)

http_archive(
    name = "closure_tdd",
    url = "https://github.com/Ubehebe/closure-tdd/archive/167e42.zip",
    strip_prefix = "closure-tdd-167e4235e6a098432b78a5f1842c83c6af905961",
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