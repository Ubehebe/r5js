def _node_impl(repository_ctx):
  repository_ctx.file("BUILD", content="""exports_files(["bin/node"], visibility = ["//visibility:public"])""")
  if repository_ctx.os.name.lower().startswith("mac os"):
    repository_ctx.download_and_extract(
            [
                "http://mirror.bazel.build/nodejs.org/dist/v6.10.2/node-v6.10.2-darwin-x64.tar.xz",
                "https://nodejs.org/dist/v6.10.2/node-v6.10.2-darwin-x64.tar.xz",
            ],
            stripPrefix = "node-v6.10.2-darwin-x64",
            sha256 = "360b887361b2597613f18968e3fc0e920079a363d0535fc4e40532e3426fc6eb",
    )
  else:
    repository_ctx.download_and_extract(
        [
            "http://mirror.bazel.build/nodejs.org/dist/v6.10.2/node-v6.10.2-linux-x64.tar.xz",
            "http://nodejs.org/dist/v6.10.2/node-v6.10.2-linux-x64.tar.xz",
        ],
        stripPrefix = "node-v6.10.2-linux-x64",
        sha256 = "b519cd616b0671ab789d2645c5c026deb7e016d73a867ab4b1b8c9ceba9c3503",
    )

node = repository_rule(
    implementation = _node_impl,
    attrs = {},
)