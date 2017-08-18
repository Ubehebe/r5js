load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

def scheme_source(name, src):
  base_name = src[:-4]
  js_name = base_name + ".js"

  native.genrule(
      name = name + "_src",
      srcs = [src],
      outs = [js_name],
      cmd = "cat > $(@) << END\n"
      + "goog.provide('" + name + "');\n"
      + "/** @const */var " + name + " = \n"
      + "END\n"
      + "cat $(<)"
      # backslash-escape backslash
      + " | sed -e 's/\\\\/\\\\\\\\/g'"
      # backslash-escape double quotes
      + " | sed -e 's/\"/\\\\\"/g'"
      # newline => \n (two characters)
      + " | sed -e 's/$$/END_OF_LINE/g'"
      + " | tr '\\n' ' '"
      # leading quote
      + " | sed -e 's/^/\"/'"
      # trailing quote and semicolon
      + " | sed -e 's/$$/\";/'"
      + " | sed -e 's/END_OF_LINE/\\\\n/g'"
      + " >> $(@)",
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

def node_test(name, src, entry_point, debug=False):

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
      + "node "
      + ("--debug-brk --inspect " if debug else "")
      + "-e " + node_cmd + "\n"
      + "END",
      outs = [
          name + ".sh",
      ],
  )

  native.sh_test(
      name = name,
      timeout = "eternal" if debug else "short",
      srcs = [
          name + ".sh",
      ],
      data = [
          name + "_copy.js",
      ],
  )