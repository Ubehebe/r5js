load("@io_bazel_rules_closure//closure:defs.bzl", "closure_js_library")

JS_VERSION = "ECMASCRIPT6_STRICT"

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
          "//:__subpackages__",
      ],
  )

def _node_test_impl(ctx):
  input = ctx.file.src

  node_cmd = """"require('./%s/%s').%s(process.argv, process.env)" type=unit verbose""" % (
    ctx.label.package,
    input.basename,
    ctx.attr.entry_point,
  )

  ctx.action(
      inputs = [input],
      outputs = [ctx.outputs.executable],
      command =
      "cat > %s << END\n#!/bin/sh\n%s %s -e %s\nEND" % (
          ctx.outputs.executable.path,
          ctx.executable._node.path,
          "--debug-brk --inspect " if ctx.attr.debug else "",
          node_cmd),
  )

  runfiles = ctx.runfiles(
      files = [input, ctx.executable._node],
  )
  return [DefaultInfo(runfiles=runfiles)]

node_test = rule(
      implementation = _node_test_impl,
      test = True,
      attrs = {
          "src": attr.label(mandatory = True, allow_single_file = True),
          "entry_point": attr.string(mandatory = True),
          "debug": attr.bool(),
          "_node": attr.label(
              default = Label("@node//:bin/node"),
              allow_files = True,
              executable = True,
              cfg = "host",
          ),
      },
)