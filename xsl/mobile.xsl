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
            doctype-system="about:legacy-compat"/>

    <!--Default transform is identity.-->
    <xsl:template match="node() | @*">
        <xsl:copy>
            <xsl:apply-templates select="node() | @*"/>
        </xsl:copy>
    </xsl:template>

    <!--Strip comments. -->
    <xsl:template match="comment()" />

    <!--Strip Facebook and Twitter buttons...-->
        <xsl:template match="//div[@id='social']" />

    <!--...but instead append them below the navs.-->
    <xsl:variable name="social">
        <xsl:apply-templates select="//div[@id='social']/*"/>
    </xsl:variable>

    <!--Strip nav bar...-->
    <xsl:template match="nav" />

    <!--...but instead put a copy next to each section.-->
    <xsl:variable name="nav">
        <div class="content-secondary">
            <ul data-role="listview">
                <xsl:apply-templates select='//nav/ul/*'/>
            </ul>
            <xsl:copy-of select="$social"/>
        </div>
    </xsl:variable>

   <!--Inject mobile CSS and JavaScript.-->
    <xsl:template match="head">
        <head>
            <xsl:apply-templates/>
            <link
                    rel="stylesheet"
                    href="//code.jquery.com/mobile/1.1.0-rc.2/jquery.mobile-1.1.0-rc.2.min.css"/>
            <link
                    rel="stylesheet"
                    href="mobile.css"/>
            <script
                    src="//code.jquery.com/jquery-1.7.1.min.js">
            </script>
            <script
                    src="//code.jquery.com/mobile/1.1.0-rc.2/jquery.mobile-1.1.0-rc.2.min.js">
            </script>
            <script
                    src="all.js">
            </script>
        </head>
    </xsl:template>

    <!--Setup the text-message-like UI.-->
    <xsl:template match='section[@id="play"]/comment()'>
        <noscript>Gay Lisp is written in JavaScript, but it looks like you have disabled JavaScript.</noscript>
        <div id="blockterm-output">
        </div>
        <input type="text" class='terminal' id='blockterm-input'/>
    </xsl:template>

    <!--For the mobile site, the spec is a different file. Make sure
    jQuery doesn't try to ajax it into the DOM. -->
    <xsl:template match='a[@href="r5rs.html"]'>
        <a href="{@href}" data-ajax="false">
            <xsl:apply-templates/>
        </a>
    </xsl:template>

    <!--Transform sections into annoying jQuery Mobile markup.-->
    <xsl:template match="section|header">
        <div id="{@id}" data-role="page" class="type-interior">
            <xsl:apply-templates select="h1" />
            <div data-role="content">
                <div class="content-primary">
                    <xsl:apply-templates select="node()[not(self::h1)]"/>
                </div>
                <!--Append the nav bar beneath each "section".-->
                <xsl:copy-of select="$nav"/>
            </div>
        </div>
    </xsl:template>

    <!--Transform headings into annoying jQuery Mobile markup.-->
    <xsl:template match="h1">
        <div data-role="header">
            <h1>Gay Lisp</h1>
        </div>
    </xsl:template>

    <!--Transform <dl>s into annoying jQuery Mobile markup. -->
    <xsl:template match="dl">
        <div data-role="collapsible-set">
            <xsl:apply-templates />
        </div>
    </xsl:template>

    <xsl:template match="dt">
        <div data-role="collapsible">
            <h1>
                <xsl:apply-templates />
            </h1>
            <xsl:apply-templates select="((following-sibling::dd)[1])/node()" />
        </div>
    </xsl:template>

    <xsl:template match="dd" />

</xsl:stylesheet>