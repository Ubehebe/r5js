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

def node_test(name, src, entry_point):

  native.genrule(
      name = name + "_copy",
      srcs = [
          src,
      ],
      outs = [
          name + "_copy.js"
      ],
      cmd = "cp $(<) $(@)",
  )

  node_require = "require('./" + PACKAGE_NAME + "/" + name + "_copy.js')"
  node_cmd = '"' + node_require + '.' + entry_point + '(process.argv, process.env)" type=unit verbose'

  native.genrule(
      name = name + "_sh",
      testonly = 1,
      srcs = [src],
      cmd = "cat > $(@) << END\n"
      + "#!/bin/sh\n"
      + "node -e " + node_cmd + "\n"
      + "END",
      outs = [
          name + ".sh",
      ],
  )

  native.sh_test(
      name = name,
      srcs = [
          name + ".sh",
      ],
      data = [
          name + "_copy.js",
      ],
  )