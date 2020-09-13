default: index.html show

header.html: README.md
	pandoc -o $@ $^ \
		--smart \
		--filter pandoc-crossref

garden-party.html: garden-party.xsl garden-party.xml
	xsltproc $^ > $@

works-cited.html: works-cited.md works-cited.bib
	pandoc -o $@ $< --smart --filter pandoc-citeproc

index.html: garden-party.html header.html works-cited.html
	sed -e '/INSERTHEADERHERE/{r header.html' -e 'd}' $< > $@
	cat works-cited.html >> $@

show: 
	gvfs-open index.html
clean: 
	rm garden-party.html header.html works-cited.html main.html 
