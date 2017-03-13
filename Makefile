debug: 
	DEBUG=metalsmith:metadata metalsmith

build: node_modules
	node_modules/.bin/metalsmith

node_modules: package.json
	npm install

.PHONY: build
