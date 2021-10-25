index.html: esny-komputumi.org revealjs.template
	pandoc -o $@ $< \
	-s -t revealjs --template=revealjs.template \
	-V revealjs-url=../../assets/js/vendor/revealjs \
	-V theme=moon
	firefox $@ &
