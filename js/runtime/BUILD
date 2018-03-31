load("@build_bazel_rules_typescript//:defs.bzl", "ts_library")

package(
    default_visibility = [
        "//js:__subpackages__",
    ],
)

ts_library(
    name = "environment",
    tsconfig = "//:tsconfig.json",
    srcs = [
        "environment.ts",
    ],
    deps = [
        "//js:value",
    ],
)

ts_library(
    name = "runtime",
    srcs = [
        "assignment.ts",
        "branch.ts",
        "call_with_current_continuation.ts",
        "continuation.ts",
        "dynamic_wind_continuation.ts",
        "environment_impl.ts",
        "eof.ts",
        "errors.ts",
        "lambda.ts",
        "nil.ts",
        "proc_call.ts",
        "procedure.ts",
        "procspec.ts",
        "top_level_assignment.ts",
        "top_level_syntax_assignment.ts",
        "trampoline_helper.ts",
        "user_defined_procedure.ts",
        "valutil.ts",
        "varargs_user_defined_procedure.ts",
    ],
    deps = [
        ":environment",
        "//js/ast:datum",
        "//js/ast:type",
        "//js/macro",
        "//js:error",
        "//js:value",
        "//js/io",
        "//js/parse:nonterminals",
        "//js/parse:rename_util",
        "//js/parse:terminals",
        "//js/parse:parser",
    ],
)

ts_library(
    name = "primitive_procedures",
    srcs = [
        "primitive_procedures.ts",
    ],
    deps = [
        ":environment",
        ":runtime",
        ":trampoline",
        "//js:value",
        "//js/ast:type",
        "//js/io",
        "//js:error",
        "//js/ast:datum",
        "//js/parse:parser_impl",
    ],
)

ts_library(
    name = "trampoline",
    srcs = [
        "trampoline.ts",
    ],
    deps = [
        "//js:value",
        "//js/ast:datum",
        "//js/io",
        "//js/parse:parser_impl",
        ":environment",
        ":runtime",
    ],
)