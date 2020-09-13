index.html: montclair.md default.revealjs
	pandoc -o $@ $< \
	-s -t revealjs \
	-V revealjs-url=../../assets/js/vendor/reveal.js \
	-V theme=solarized \
	--template default.revealjs
	chromium $@ &
