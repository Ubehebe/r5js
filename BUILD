# The default value of node_modules for ts_library targets is //:node_modules.
# This repo doesn't maintain its own package.json, instead reusing one from upstream
# (rules_typescript). So instead of passing a non-default node_modules to every ts_library in this
# repo, make //:node_modules point to the upstream one.
alias(
    name = "node_modules",
    actual = "@upstream_node_modules//:node_modules",
    visibility = [
        "//visibility:public",
    ],
)

exports_files(["tsconfig.json"])