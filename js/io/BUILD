load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = ["//js:__subpackages__"],
)

exports_files([
    "in_memory_input_port.ts",
    "port_manager.ts",
])

ts_library(
    name = "io_ts",
    srcs = glob(["*.ts"], exclude = [
        "in_memory_input_port.ts",
        "port_manager.ts",
    ]),
    deps = [
        "//js/ast:datum_ts",
        "//js:value_ts",
    ],
    tsconfig = "//:tsconfig.json",
)