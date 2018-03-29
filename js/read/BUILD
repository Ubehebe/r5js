load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)


ts_library(
    name = "read",
    srcs = glob(["*.ts"]),
    deps = [
        "//js/ast:datum",
        "//js/parse:nonterminals",
        "//js/parse:terminals",
        "//js:error",
        "//js/scan",
    ],
)
