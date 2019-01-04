default:
	npx metalsmith -c dev.json

debug:
	npx DEBUG=metalsmith:metadata metalsmith

prod:
	npx metalsmith
