build:
	spago bundle-app
	rm -f dist/*.js dist/*.map
	NODE_OPTIONS='--max-old-space-size=4096' exec npx parcel build index.html

.PHONY: build