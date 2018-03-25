load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

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

closure_ts_library(
    name = "io",
    deps = [
        ":io_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INEXISTENT_PROPERTY",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_JSDOC_MISSING_TYPE_WARNING",
        "JSC_UNKNOWN_EXPR_TYPE",
        "JSC_UNKNOWN_OVERRIDE",
        "JSC_UNRECOGNIZED_TYPE_ERROR",
    ],
)