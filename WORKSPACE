load("@bazel_tools//tools/build_defs/repo:git.bzl", "git_repository", "new_git_repository")

git_repository(
    name = "r5js",
    remote = "https://github.com/Ubehebe/r5js.git",
    commit = "76434f25863273adf62f415f1a31dd3f7f80669a",
)

new_git_repository(
    name = "spec",
    remote = "https://github.com/Ubehebe/r5rs-html5.git",
    commit = "b9fe77910e60519491097f66e7e7b1bcd2ce0764",
    build_file = "//:BUILD.spec",
)
