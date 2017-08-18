load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

closure_js_library(
    name = "node_externs",
    srcs = [
        "externs/core.js",
        "externs/events.js",
        "externs/process.js",
        "externs/readline.js",
    ],
    visibility = [
        "//js/platform/node:__pkg__",
    ],
    suppress = [
        "JSC_JSDOC_MISSING_TYPE_WARNING", # core.js
    ],
)