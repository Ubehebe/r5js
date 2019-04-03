def _embed_template_literal(ctx):
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
                  " >> $2; echo '`;' >> $2",
        arguments = [args],
        progress_message = "embedding %s into ts string literal" % ctx.file.src,
    )

    return [
        DefaultInfo(
            files = depset([out]),
        ),
    ]

embed_template_literal = rule(
    implementation = _embed_template_literal,
    doc = """embeds an entire file inside a JavaScript template literal.
The output is a valid TypeScript file that looks like this:
const NAME = `
<file contents>
`;
where NAME is the name of this embed_template_literal target.""",
    attrs = {
        "src": attr.label(
            mandatory = True,
            allow_single_file = [".scm"],
        ),
    },
)
