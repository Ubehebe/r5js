load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library", "closure_js_test")

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
        "//src/js/test/matchers",
        "//src/scm:TEST_FRAMEWORK",
        "//src/scm:TEST_FRAMEWORK_TESTS",
        "//src/scm:R5RS_TESTS",
        "//src/scm:NEGATIVE_TESTS",
        "//src/scm:OTHER_TESTS",
    ],
    suppress = [
        "JSC_MISSING_JSDOC",
        "CR_NOT_PROVIDED", # main.js requires r5js.curPlatform
    ],
    language = "ECMASCRIPT6_STRICT",
    visibility = [
        "//src/js:__subpackages__",
    ]
)

closure_js_test(
    name = "test",
    language = "ECMASCRIPT6_STRICT",
    srcs = [
        "main_test.js",
    ],
    deps = [
        ":lib",
        "@io_bazel_rules_closure//closure/library:testing",
    ],
    entry_points = [
        "goog:r5js.test.main_test",
    ],
)