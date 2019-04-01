package(default_visibility = [
    "//:__pkg__",
])

genrule(
    name = "index_desktop",
    srcs = [
        "desktop.xsl",
        "//ui:index.html",
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
        "//ui:index.html",
    ],
    outs = [
        "index_mobile.html",
    ],
    cmd = "xsltproc $(SRCS) > $(@)",
)
