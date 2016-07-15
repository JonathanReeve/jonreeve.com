index.html: dh2016-presentation.md impress-template.html
	pandoc -o $@ $< \
	--template impress-template.html \
	--mathjax \
	-V impress-url=impress.js \
	-s -t html5 --section-divs
	chromium $@ &
