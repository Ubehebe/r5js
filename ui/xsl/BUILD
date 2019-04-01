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

genrule(
    name = "index_mobile",
    srcs = [
        "mobile.xsl",
        "index.html",
    ],
    outs = [
        "index_mobile.html",
    ],
    cmd = "xsltproc $(SRCS) > $(@)",
)
