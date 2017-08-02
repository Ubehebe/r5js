# Note: bazel test fails because of many missing resource files.
# The fundamental problem is that r5js uses java as a build system instead of bazel.
# Delete all the custom compilation infrastructure.
java_test(
    name = "CompilationTest",
    srcs = [
        "CompilationTest.java",
    ],
    deps = [
        "@my_guava//jar",
        "@truth//jar",
        "//src/java/r5js",
    ],
)

java_test(
    name = "NashornTest",
    srcs = [
        "NashornTest.java",
    ],
    deps = [],
)

java_test(
    name = "NodeTest",
    srcs = [
        "NodeTest.java",
    ],
    deps = [
        "@truth//jar",
        "//src/java/r5js",
    ],
)

# This doesn't compile yet because of weird javafx deps in R5JSTests.
#java_test(
#    name = "R5JSTests",
#    srcs = [
#        "R5JSTests.java",
#    ],
#    deps = [
#        "@my_guava//jar",
#        "//src/java/r5js",
#        "//src/java/r5js/platform/nashorn",
#    ],
#)