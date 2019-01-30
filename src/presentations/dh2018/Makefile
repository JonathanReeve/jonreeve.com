index.html: dh2018.md default.revealjs
	pandoc -o $@ $< \
	-s -t revealjs \
  --dpi 196 \
	-V revealjs-url=../../assets/js/vendor/reveal.js \
	-V theme=solarized \
	--template default.revealjs
	chromium $@ &
