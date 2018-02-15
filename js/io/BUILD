load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "io",
    srcs = [],
    suppress = [
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
    visibility = ["//js:__subpackages__"],
    exports = [
        ":callback_backed_port",
        ":in_memory_input_port",
        ":in_memory_output_port",
        ":in_memory_port_buffer",
        ":input_port",
        ":output_port",
        ":output_saving_port",
        ":port_manager",
    ],
)

closure_js_library(
    name = "callback_backed_port",
    srcs = [
        "callback_backed_port.js",
    ],
    deps = [
        ":output_port",
    ],
)

closure_js_library(
    name = "in_memory_input_port",
    srcs = [
        "in_memory_input_port.js",
    ],
    deps = [
        ":in_memory_port_buffer",
        ":input_port",
        "//js/ast:character",
        "//js/ast:datum",
        "//js/read",
        "//js/scan",
    ],
)

closure_js_library(
    name = "in_memory_output_port",
    srcs = [
        "in_memory_output_port.js",
    ],
    deps = [
        ":in_memory_port_buffer",
        ":output_port",
        ":output_saving_port",
    ],
)

ts_library(
    name = "in_memory_port_buffer_ts",
    srcs = [
        "in_memory_port_buffer.ts",
    ],
)

closure_ts_library(
    name = "in_memory_port_buffer",
    deps = [
        ":in_memory_port_buffer_ts",
    ],
)

closure_js_library(
    name = "input_port",
    srcs = [
        "input_port.js",
    ],
    deps = [
        "//js/ast:character",
    ],
)

ts_library(
    name = "output_port_ts",
    srcs = [
        "output_port.ts",
    ],
)

closure_ts_library(
    name = "output_port",
    deps = [
        ":output_port_ts",
    ],
    suppress = [
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_UNKNOWN_EXPR_TYPE",
    ]
)

ts_library(
    name = "output_saving_port_ts",
    srcs = [
        "output_saving_port.ts",
    ],
    deps = [
        ":output_port_ts",
    ],
)

closure_ts_library(
    name = "output_saving_port",
    deps = [
        "output_saving_port_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "port_manager",
    srcs = [
        "port_manager.js",
    ],
    deps = [
        ":in_memory_input_port",
        ":in_memory_output_port",
        ":in_memory_port_buffer",
        ":input_port",
        ":output_port",
    ],
)
