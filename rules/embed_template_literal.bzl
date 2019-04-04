def _embed_template_literal(ctx):
    ctx.actions.run_shell(
        inputs = [ctx.file.src],
        outputs = [ctx.outputs.ts],
        tools = [ctx.executable._embed_template_literal],
        command = "< %s %s %s > %s" % (
            ctx.file.src.path,
            ctx.executable._embed_template_literal.path,
            ctx.attr.name,
            ctx.outputs.ts.path,
        ),
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
        "_embed_template_literal": attr.label(
            default = "//scripts:embed_template_literal",
            executable = True,
            cfg = "host",
        ),
    },
    outputs = {
        "ts": "%{name}.ts",
    },
)
