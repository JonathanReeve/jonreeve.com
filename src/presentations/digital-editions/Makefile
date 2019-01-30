index.html: pres.md 
	pandoc -o $@ $< \
	-s -t revealjs \
	-V revealjs-url=../../assets/js/vendor/reveal.js \
	-V theme=moon \
	--template default.revealjs
	chromium $@ &
