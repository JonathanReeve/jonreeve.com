index.html: joyce-editions.md default.revealjs
	pandoc -o $@ $< \
	-s -t revealjs \
	-V revealjs-url=../../assets/js/vendor/reveal.js \
	-V theme=moon \
	--template default.revealjs
	chromium $@ &
