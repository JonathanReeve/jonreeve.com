<?xml version="1.0" encoding="UTF-8"?> 
<xsl:stylesheet version="1.0" xmlns:xsl="http://www.w3.org/1999/XSL/Transform"> 

<!-- The Entire HTML Web Page --> 
<xsl:template match="/"> 
	<html>
		<head>
			<link rel="stylesheet" href="garden-party.css" />
			<link href='https://fonts.googleapis.com/css?family=Merriweather:400,400italic' rel='stylesheet' type='text/css'/>
			<script src="jquery-2.1.4.min.js"></script>
			<script src="custom.js"></script>
		</head>
		<body>
			<xsl:apply-templates/>
		</body>
	</html>
</xsl:template> 

<!--The Entire Set of Text/Commentary Pairs -->
<xsl:template match="TEI/text/body">
	<div id="textBody">
		<table>
			<tr>
				<th>Text</th>
				<th>Commentary</th>
			</tr>
			<xsl:apply-templates/>
		</table>
	</div> <!--End of Container-->
</xsl:template>

<!-- Text/Commentary Pairs --> 
<xsl:template match="ab">
	<tr class="interp"><xsl:apply-templates/></tr>
</xsl:template>

<!-- Metadata --> 
<xsl:template match="TEI/teiHeader">
	<div class="metadata">
		<xsl:apply-templates/>
	</div> <!--end of sourceDesc-->
</xsl:template>

<!-- Text Segments --> 
<xsl:template match="seg">
	<td class="lexeme">
		<xsl:attribute name="id">
			<xsl:value-of select="@n"/>
		</xsl:attribute>
		<xsl:if test="@type = 'startPara' "> 
			<span class="pilcrow">¶</span>
		</xsl:if> 
		<xsl:apply-templates select="@n | node()"/>
	</td>
</xsl:template>

<!-- Lexia IDs --> 
<xsl:template match="@n">
	<sup class="lexeme-id">
		<xsl:value-of select="."/>
	</sup>
</xsl:template> 

<!-- Commentary --> 
<xsl:template match="interp">
	<td class="interp">
		<xsl:apply-templates/>
	</td>
</xsl:template>

<!-- Tags --> 
<xsl:template match="interp/desc">
	<xsl:for-each select=".">
		<span>
			<xsl:attribute name="class">tag 
				<xsl:value-of select="."/>
			</xsl:attribute>
			<xsl:value-of select="."/>
		</span>
	</xsl:for-each>
</xsl:template> 

<!-- Handle links --> 
<xsl:template match="ptr">
	<a>
		<xsl:attribute name="href">
			<xsl:value-of select="@target"/>
		</xsl:attribute>
		<xsl:apply-templates/>
	</a>
</xsl:template> 

<!-- Title -->
<xsl:template match="head[@type='title']">
	<h1><xsl:apply-templates/></h1>
</xsl:template>

<!-- Byline -->
<xsl:template match="head[@type='byline']">
	<h2><xsl:apply-templates/></h2>
</xsl:template>

<!-- Verse Stanzas --> 
<xsl:template match="lg">
	<p class="song">
		<xsl:apply-templates/>
	</p>
</xsl:template>

<!-- Verse Lines --> 
<xsl:template match="l">
	<xsl:apply-templates/><br/>
</xsl:template>

<!-- Emphasized Text (Italics) --> 
<xsl:template match="emph">
	<span class="emph"><xsl:apply-templates/></span>
</xsl:template>

<xsl:template match="foreign">
	<span class="emph"><xsl:apply-templates/></span>
</xsl:template>

<!-- This inserts a placeholder which sed will swap out with the contents of header.html. -->
<xsl:template match="div[@type='header']">
	<div class="header-area">
		INSERTHEADERHERE
	</div> 
</xsl:template>

</xsl:stylesheet> 
