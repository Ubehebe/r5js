def _embed_template_literals(ctx):
    tmp_outputs = []

    # Transform each input blah.txt to a TypeScript file containing a single binding,
    # export const BLAH = `<file>`;
    # TODO: this will fail (at tsc compilation time) if inputs differ only in their extension.
    for src in ctx.files.srcs:
        tmp_out = ctx.actions.declare_file(src.basename + ".tmp")
        var_name = src.basename[:-len(src.extension) - 1].upper().replace("-", "_")
        ctx.actions.run_shell(
            inputs = [src],
            outputs = [tmp_out],
            tools = [ctx.executable._embed_template_literal],
            command = "< %s %s %s > %s" % (
                src.path,
                ctx.executable._embed_template_literal.path,
                var_name,
                tmp_out.path,
            ),
            progress_message = "embedding %s into template literal" % src,
        )
        tmp_outputs.append(tmp_out)

    # Now cat all the bindings together to form the final output.
    ctx.actions.run_shell(
        inputs = tmp_outputs,
        outputs = [ctx.outputs.ts],
        command = "cat %s > %s" % (
            " ".join([tmp.path for tmp in tmp_outputs]),
            ctx.outputs.ts.path,
        ),
    )

    return [DefaultInfo()]

embed_template_literals = rule(
    implementation = _embed_template_literals,
    doc = """embeds arbitrary files into JavaScript template literals.
The output is a single TypeScript file containing one exported binding for each source file.
A source file blah.whatever generates a binding like this: export const BLAH = `<contents>`.""",
    attrs = {
        "srcs": attr.label_list(
            allow_empty = False,
            allow_files = True,
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
