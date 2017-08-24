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
  # bazel 0.5.3: ctx.actions.declare_file
  copy = ctx.new_file("%s.copy" % input.basename)
  # bazel 0.5.3: ctx.actions.run_shell
  ctx.action(
    inputs = [input],
    outputs = [copy],
    command = "cp %s %s" % (input.path, copy.path),
  )

  node_require = "require('./%s/%s')" % (ctx.label.package, copy.basename)
  node_cmd = '"' + node_require + '.' + ctx.attr.entry_point + '(process.argv, process.env)" type=unit verbose'

  ctx.action(
      inputs = [copy],
      outputs = [ctx.outputs.executable],
      command = ("cat > %s << END\n"
      + "#!/bin/sh\n"
      + "node "
      + ("--debug-brk --inspect " if ctx.attr.debug else "")
      + "-e %s\n"
      + "END") % (ctx.outputs.executable.path, node_cmd),
  )

  runfiles = ctx.runfiles(
      files = [copy],
  )
  return [DefaultInfo(runfiles=runfiles)]

node_test = rule(
      implementation = _node_test_impl,
      test = True,
      attrs = {
          "src": attr.label(mandatory = True, allow_single_file = True),
          "entry_point": attr.string(mandatory = True),
          "debug": attr.bool(),
      },
)