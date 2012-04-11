<xsl:stylesheet
        version="1.0"
        xmlns:xsl="http://www.w3.org/1999/XSL/Transform">

    <xsl:strip-space elements="*"/>

    <!--XSLT processors generally don't know about HTML5, so
    we have to have some kludges. For xsltproc, setting method to "xml"
    prevents it from annoyingly inserting an http-equiv header. Setting
     doctype-system to "about: legacy-compat" gets the generated doctype
     to a legacy string considered valid by HTML5. (Generating the short-form
     <!DOCTYPE html> is apparently only possible in XSLT via a kludge.) -->
    <xsl:output
            method="xml"
            encoding="UTF-8"
            omit-xml-declaration="yes"
            doctype-system="about:legacy-compat"/>

    <!--Default transform is identity.-->
    <xsl:template match="node() | @*">
        <xsl:copy>
            <xsl:apply-templates select="node() | @*"/>
        </xsl:copy>
    </xsl:template>

    <!--Strip comments. -->
    <xsl:template match="comment()"/>

    <!--Strip header.-->
    <xsl:template match="header"/>

    <!--Inject CSS and JavaScript.-->
    <xsl:template match="head">
        <head>
            <xsl:apply-templates/>
            <link rel="stylesheet" href="r5rs.css"/>
            <script src="all.js">
                <xsl:comment/>
            </script>
        </head>
    </xsl:template>

    <!--Style nav bar...-->
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
        <textarea class='terminal' id='play' rows="20">
            <xsl:comment/>
        </textarea>
    </xsl:template>

    <!--Transform sections into divs for now.-->
    <xsl:template match="section">
        <div id="{@id}">
            <xsl:apply-templates/>
        </div>
    </xsl:template>

    <!--Make hero text.-->
    <xsl:template match="//header/h1">
        <h1 class="hero">
            <xsl:apply-templates/>
        </h1>
    </xsl:template>

    <!--Fix links. -->
    <xsl:template match="a[starts-with(@href, '#')]">
        <a href="{@href}" class="vis-manager">
            <xsl:apply-templates/>
        </a>
    </xsl:template>

    <xsl:template match="a[@href='r5rs.html']">
        <a href="#spec"
           class="vis-manager"
           data-prevent-default="true">
            <xsl:apply-templates/>
        </a>
    </xsl:template>

</xsl:stylesheet>