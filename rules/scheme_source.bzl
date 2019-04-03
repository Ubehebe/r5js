def _scheme_source(ctx):
    out = ctx.actions.declare_file(ctx.file.src.basename[:-4] + ".ts")
    args = ctx.actions.args()
    args.add(ctx.file.src)
    args.add(out)

    # TODO: make this an external shell script.
    ctx.actions.run_shell(
        inputs = [ctx.file.src],
        outputs = [out],
        command = "cat > $2 << END\n" +
                  "export const " + ctx.attr.name + " = \`\n" +
                  "END\n" +
                  # backslash-escape backslash
                  "< $1 sed -e 's/\\\\/\\\\\\\\/g'" +
                  # backslash-escape backticks
                  " | sed -e 's/`/\\\\`/g'" +
                  " >> $2; echo '`' >> $2",
        arguments = [args],
        progress_message = "embedding %s into ts string literal" % ctx.file.src,
    )

    return [
        DefaultInfo(
            files = depset([out]),
        ),
    ]

scheme_source = rule(
    implementation = _scheme_source,
    attrs = {
        "src": attr.label(
            mandatory = True,
            allow_single_file = [".scm"],
        ),
    },
)
