load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

def scheme_source(name, src):
  base_name = src[:-4]
  js_name = base_name + ".js"
  native.genrule(
      name = name + "_src",
      srcs = [src],
      outs = [js_name],
      tools = [
          "//src/java/r5js:SourceWriter",
      ],
      cmd = "$(location //src/java/r5js:SourceWriter) $< " + name + " > $@",
  )

  closure_js_library(
      name = name,
      srcs = [
          ":" + name + "_src",
      ],
      visibility = [
          "//src:__subpackages__",
      ],
  )

