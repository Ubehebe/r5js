load("//:builddefs.bzl", "node_test")
load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_binary", "closure_js_library")

closure_js_library(
    name = "lib",
    testonly = 1,
    srcs = glob(["*.js"], exclude = ["main_test.js"]),
    deps = [
        "@io_bazel_rules_closure//closure/library",
        "@io_bazel_rules_closure//closure/library:testing",
        "@closure_tdd//:closure-tdd",
        "//src/js:bbom",
        "//src/js/platform/common",
        "//src/js/platform/node",
        "//src/js/test/matchers",
        "//src/scm:TEST_FRAMEWORK",
        "//src/scm:TEST_FRAMEWORK_TESTS",
        "//src/scm:R5RS_TESTS",
        "//src/scm:NEGATIVE_TESTS",
        "//src/scm:OTHER_TESTS",
    ],
    suppress = [
        "JSC_MISSING_JSDOC",
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//src/js:__subpackages__",
    ]
)

closure_js_binary(
    name = "bin",
    testonly = 1,
    debug = True,
    deps = [
        ":lib",
    ],
    compilation_level = "simple",
    entry_points = [
        "goog:r5js.test.main",
    ],
)

node_test(
    name = "test",
#    debug = True,
    src = ":bin.js",
    entry_point = "r5js.test.main",
)