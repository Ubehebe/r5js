def _embed_template_literal(ctx):
    args = ctx.actions.args()
    args.add(ctx.file.src)
    args.add(ctx.outputs.ts)

    # TODO: make this an external shell script.
    ctx.actions.run_shell(
        inputs = [ctx.file.src],
        outputs = [ctx.outputs.ts],
        command = "cat > $2 << END\n" +
                  "export const " + ctx.attr.name + " = \`\n" +
                  "END\n" +
                  # backslash-escape backslash
                  "< $1 sed -e 's/\\\\/\\\\\\\\/g'" +
                  # backslash-escape backticks
                  " | sed -e 's/`/\\\\`/g'" +
                  " >> $2; echo '`;' >> $2",
        arguments = [args],
        progress_message = "embedding %s into template literal" % ctx.file.src,
    )

    return [DefaultInfo()]

embed_template_literal = rule(
    implementation = _embed_template_literal,
    doc = """embeds an entire file inside a JavaScript template literal.
The output is a valid TypeScript file that looks like this:
export const NAME = `
<file contents>
`;
where NAME is the name of this embed_template_literal target.""",
    attrs = {
        "src": attr.label(
            mandatory = True,
            allow_single_file = True,
        ),
    },
    outputs = {
        "ts": "%{name}.ts",
    },
)
