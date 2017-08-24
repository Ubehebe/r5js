load("//:node.bzl", "node")

node(name = "node")

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

load("@io_bazel_rules_closure//closure:defs.bzl", "closure_repositories")

closure_repositories()
