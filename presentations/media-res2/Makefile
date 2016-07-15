pres.html : pres.md
	pandoc -o index.html pres.md \
	-t revealjs \
	-V theme=moon \
	--standalone
	gvfs-open index.html
