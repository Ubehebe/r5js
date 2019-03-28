def _scheme_source(ctx):
    out = ctx.actions.declare_file(ctx.file.src.basename[:-4] + ".ts")
    args = ctx.actions.args()
    args.add(ctx.file.src)
    args.add(out)

    # TODO investigate using `template strings`. Would be nice to ship the sources non-mangled.
    # TODO: make this an external shell script.
    ctx.actions.run_shell(
        inputs = [ctx.file.src],
        outputs = [out],
        command = "cat > $2 << END\n" +
                  "export const " + ctx.attr.name + " = \n" +
                  "END\n" +
                  # backslash-escape backslash
                  "< $1 sed -e 's/\\\\/\\\\\\\\/g'" +
                  # backslash-escape double quotes
                  " | sed -e 's/\"/\\\\\"/g'" +
                  # newline => \n (two characters)
                  " | sed -e 's/$/END_OF_LINE/g'" +
                  " | tr '\\n' ' '" +
                  # leading quote
                  " | sed -e 's/^/\"/'" +
                  # trailing quote and semicolon
                  " | sed -e 's/$/\";/'" +
                  " | sed -e 's/END_OF_LINE/\\\\n/g'" +
                  " >> $2",
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
