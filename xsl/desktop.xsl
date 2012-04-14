<xsl:stylesheet
        version="1.0"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:strip-space elements="*"/>

    <!--XSLT processors generally don't know about HTML5, and HTML5
    generally isn't valid XML, so there are going to be some kludges.

    xsltproc annoyingly inserts an http-equiv <meta> tag when method is "html".
    To disable this, we could change method to "xml", but this has an even
    worse consequence: empty tags like <script ...></script> will get into the
    generated HTML as <script ... />, which some browsers unfathmoably
    can't handle. So we choose method="html".

     doctype-system="about: legacy-compat" gets the generated doctype
     to a legacy string considered valid by HTML5. (Generating the short-form
     <!DOCTYPE html> is apparently only possible in XSLT via a kludge.) -->
    <xsl:output
            method="html"
            encoding="UTF-8"
            omit-xml-declaration="yes"
            indent="no"
            doctype-system="about:legacy-compat"/>

    <!--Default transform is identity.-->
    <xsl:template match="node() | @*">
        <xsl:copy>
            <xsl:apply-templates select="node() | @*"/>
        </xsl:copy>
    </xsl:template>

    <!--Strip comments. -->
    <xsl:template match="comment()"/>

    <!--Strip header...-->
    <xsl:template match="header"/>

    <!--...but save it for later insertion in the about section. -->
    <xsl:variable name="header">
        <xsl:apply-templates select="//header/*" />
    </xsl:variable>

    <!--Inject CSS and JavaScript.-->
    <xsl:template match="head">
        <head>
            <xsl:apply-templates/>
            <link rel="stylesheet" href="r5rs.css"/>
            <script src="all.js">
            </script>
        </head>
    </xsl:template>

    <!--Style nav bar-->
    <xsl:template match="nav">
        <div id="nav">
            <span id="logo">gλ</span>
            <ul id="navlist">
                <xsl:apply-templates select="//nav/ul/*"/>
            </ul>
        </div>
    </xsl:template>

    <!--Setup the terminal UI.-->
    <xsl:template match='section[@id="play"]'>
        <noscript>Gay Lisp is written in JavaScript, but it looks like you have disabled JavaScript.</noscript>
        <textarea class='terminal' id='play' rows="20" spellcheck="false" autofocus="autofocus">
        </textarea>
    </xsl:template>

    <!--Make hero text.-->
    <xsl:template match="//header/h1|//section/h1">
        <h1 class="hero title shiny">
            <xsl:apply-templates/>
        </h1>
    </xsl:template>

    <xsl:template match="//section[@id='about']/h1">
        <xsl:copy-of select="$header"/>
    </xsl:template>

    <!--Fix links. -->
    <xsl:template match="a[starts-with(@href, '#')]">
        <a href="{@href}" class="vis-manager">
            <xsl:apply-templates/>
        </a>
    </xsl:template>

    <xsl:template match="a[@href='r5rs.html']">
        <a href="#spec" class="vis-manager">
            <xsl:apply-templates/>
        </a>
    </xsl:template>

</xsl:stylesheet>