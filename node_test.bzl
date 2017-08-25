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