http_archive(
    name = "io_bazel_rules_closure",
    strip_prefix = "rules_closure-0.4.1",
    sha256 = "ba5e2e10cdc4027702f96e9bdc536c6595decafa94847d08ae28c6cb48225124",
    url = "http://mirror.bazel.build/github.com/bazelbuild/rules_closure/archive/0.4.1.tar.gz",
)

http_archive(
    name = "closure_tdd",
    url = "https://github.com/Ubehebe/closure-tdd/archive/64dcc3.zip",
    strip_prefix = "closure-tdd-64dcc323ad4ae0afe08cfebf4f6929b81cc936aa",
)

load("@io_bazel_rules_closure//closure:defs.bzl", "closure_repositories")

closure_repositories()
