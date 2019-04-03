package(default_visibility = [
    "//:__subpackages__",
])

genrule(
    name = "index_desktop",
    srcs = [
        "desktop.xsl",
        "index.html",
    ],
    outs = [
        "index_desktop.html",
    ],
    cmd = "xsltproc $(SRCS) > $(@)",
)
