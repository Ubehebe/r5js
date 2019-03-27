genrule(
    name = "spec_xhtml",
    srcs = ["@spec//:r5rs.html"],
    outs = ["r5rs.xhtml"],
    # The spec takes a long time to parse, delaying the load and DOMContentLoaded events, so we
    # bring it in via ajax instead. Unfortunately, that means it has to be valid XHTML. I've
    # carefully ensured spec/r5rs.html is valid HTML5 and (with the addition of the following line)
    # XHTML(5), but perhaps we should run xmllint during the build process.
    cmd = """echo '<?xml version="1.0" encoding="UTF-8"?>' > $(@)
cat $(<) >> $(@)""",
)

filegroup(
    name = "spec",
    srcs = [
        "r5rs.xhtml",
        "robots.txt",
        "rotary_nav.js",
        "@spec//:images",
        "css/r5rs.css",
    ]
)

genrule(
    name = "index_desktop",
    srcs = [
        "xsl/desktop.xsl",
        "src/index.html",
    ],
    outs = [
        "index_desktop.html",
    ],
    cmd = "xsltproc $(SRCS) > $(@)",
)

genrule(
    name = "index_mobile",
    srcs = [
        "xsl/mobile.xsl",
        "src/index.html",
    ],
    outs = [
        "index_mobile.html",
    ],
    cmd = "xsltproc $(SRCS) > $(@)",
)
