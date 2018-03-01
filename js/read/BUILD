load("@build_bazel_rules_typescript//:defs.bzl", "ts_library", "closure_ts_library")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

# Workaround to avoid duplication of the AST classes in js/read execroot.
# Instead of polluting the commit history by moving in_memory_input_port.ts into this package,
# just copy it here temporarily via a genrule. TODO: remove.
genrule(
    name = "ugh",
    srcs = [
        "//js/io:in_memory_input_port.ts",
    ],
    outs = [
        "in_memory_input_port.ts",
    ],
    cmd = "cp $(<) $(@)",
)

ts_library(
    name = "read_ts",
    srcs = [
        "grammar.ts",
        "in_memory_input_port.ts",
        "reader.ts",
        "rule.ts",
        "rule_factory.ts",
    ],
    deps = [
        "//js/ast:datum_ts",
        "//js/io:in_memory_port_buffer_ts",
        "//js/io:input_port_ts",
        "//js/parse:nonterminals_ts",
        "//js/parse:terminals_ts",
        "//js/scan:scan_ts",
        "//js:error_ts",
    ],
    tsconfig = "//:tsconfig.json",
)

closure_ts_library(
    name = "shim",
    deps = [
        ":read_ts",
    ],
    suppress = [
        "CR_NOT_PROVIDED",
        "JSC_INVALID_CLOSURE_CALL_ERROR",
        "JSC_UNKNOWN_EXPR_TYPE",
    ],
)

closure_js_library(
    name = "read",
    srcs = [],
    exports = [
        ":shim",
    ],
)