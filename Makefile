default:
	metalsmith -c dev.json

debug:
	DEBUG=metalsmith:metadata metalsmith

prod:
	metalsmith
