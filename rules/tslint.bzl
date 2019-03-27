def _tslint(ctx):
    ctx.actions.write(
        ctx.outputs.executable,
        # See https://github.com/bazelbuild/bazel/issues/4054 and
        # https://github.com/bazelbuild/bazel/blob/master/tools/bash/runfiles/runfiles.bash#L32.
        # TODO: why isn't this required for :googlejavaformat?
        """
#!/bin/bash
# --- begin runfiles.bash initialization ---
       # Copy-pasted from Bazel's Bash runfiles library (tools/bash/runfiles/runfiles.bash).
       set -euo pipefail
       if [[ ! -d "${RUNFILES_DIR:-/dev/null}" && ! -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
         if [[ -f "$0.runfiles_manifest" ]]; then
           export RUNFILES_MANIFEST_FILE="$0.runfiles_manifest"
         elif [[ -f "$0.runfiles/MANIFEST" ]]; then
           export RUNFILES_MANIFEST_FILE="$0.runfiles/MANIFEST"
         elif [[ -f "$0.runfiles/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
           export RUNFILES_DIR="$0.runfiles"
         fi
       fi
       if [[ -f "${RUNFILES_DIR:-/dev/null}/bazel_tools/tools/bash/runfiles/runfiles.bash" ]]; then
         source "${RUNFILES_DIR}/bazel_tools/tools/bash/runfiles/runfiles.bash"
       elif [[ -f "${RUNFILES_MANIFEST_FILE:-/dev/null}" ]]; then
         source "$(grep -m1 "^bazel_tools/tools/bash/runfiles/runfiles.bash " \
                   "$RUNFILES_MANIFEST_FILE" | cut -d ' ' -f 2-)"
       else
         echo >&2 "ERROR: cannot find @bazel_tools//tools/bash/runfiles:runfiles.bash"
         exit 1
       fi
       # --- end runfiles.bash initialization ---
GIT_LS_OPTIONS="--modified --others"
while getopts 'a' OPT; do
    case "$OPT" in
        a) GIT_LS_OPTIONS= ;;
    esac
done
git -C "$BUILD_WORKSPACE_DIRECTORY" ls-files $GIT_LS_OPTIONS --exclude-standard --directory \
    | grep \.ts$ \
    | sed -e "s|^|${BUILD_WORKSPACE_DIRECTORY}\/|" \
    | xargs %s --config %s --fix""" %
        (ctx.executable._tslint.short_path, ctx.file.config.short_path),
        is_executable = True,
    )

    return [
        DefaultInfo(
            runfiles = ctx.runfiles(
                files = [ctx.file.config],
                transitive_files = ctx.attr._tslint[DefaultInfo].default_runfiles.files,
            ),
        ),
    ]

tslint = rule(
    implementation = _tslint,
    executable = True,
    attrs = {
        "_tslint": attr.label(
            default = "@npm//tslint/bin:tslint",
            executable = True,
            cfg = "host",
        ),
        "config": attr.label(
            mandatory = True,
            allow_single_file = [".json"],
        ),
    },
)
