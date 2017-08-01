load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_binary", "closure_js_library")

closure_js_library(
    name = "js",
    testonly = 1,
    srcs = glob(
        ["**/*.js"],
        # TODO: remove tdd submodule
        exclude = glob(["tdd/**/*.js"]),
    ),
    language = "ECMASCRIPT6_STRICT",
    deps = [
        "@io_bazel_rules_closure//closure/library",
    ],
    visibility = [
        "//visibility:public",
    ],
    # TODO: gradually remove these
    suppress = [
        "CR_DUPLICATE_PROVIDES",
        "CR_NOT_PROVIDED",
        "JSC_MISSING_JSDOC",
        "JSC_MUST_BE_PRIVATE",
        "JSC_PRIMITIVE_OBJECT",
    ],
)

closure_js_binary(
    name = "bin",
    testonly = 1,
    deps = [
        ":js",
    ],
)
