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
    name = "NodeTest",
    srcs = [
        "NodeTest.java",
    ],
    deps = [
        "@truth//jar",
        "//src/java/r5js",
    ],
)