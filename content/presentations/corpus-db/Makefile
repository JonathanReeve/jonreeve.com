index.html: corpus-db.md default.revealjs
	pandoc -o $@ $< \
	-s -t revealjs \
	-V revealjs-url=/assets/js/vendor/reveal.js \
	-V theme=league
	chromium $@ &
