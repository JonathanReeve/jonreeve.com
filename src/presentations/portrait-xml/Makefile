index.html: portrait-presentation.md impress-template.html
	pandoc -o $@ $< \
	--template impress-template.html \
	--mathjax \
	-V impress-url=../../assets/js/vendor/impress.js \
	-s -t html5 --section-divs
	chromium $@ &
