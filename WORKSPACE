http_archive(
    name = "io_bazel_rules_closure",
    strip_prefix = "rules_closure-0.4.1",
    sha256 = "ba5e2e10cdc4027702f96e9bdc536c6595decafa94847d08ae28c6cb48225124",
    url = "http://mirror.bazel.build/github.com/bazelbuild/rules_closure/archive/0.4.1.tar.gz",
)

http_archive(
    name = "closure_tdd",
    url = "https://github.com/Ubehebe/closure-tdd/archive/5e020f.zip",
    strip_prefix = "closure-tdd-5e020fdd9b1b6171b3978b1f70e09f9cc8806c5a",
)

maven_jar(
    name = "my_closure_compiler",
    artifact = "com.google.javascript:closure-compiler:v20150315",
)

maven_jar(
    name = "closure_compiler_externs",
    artifact = "com.google.javascript:closure-compiler-externs:v20150315",
)

maven_jar(
    name = "my_guava",
    artifact = "com.google.guava:guava:18.0",
)

maven_jar(
    name = "truth",
    artifact = "com.google.truth:truth:0.23",
)

maven_jar(
    name = "junit",
    artifact = "junit:junit:4.11",
)

load("@io_bazel_rules_closure//closure:defs.bzl", "closure_repositories")

closure_repositories()
