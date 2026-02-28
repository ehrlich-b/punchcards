.PHONY: run test

run:
	@echo "Serving at http://localhost:8029"
	python3 -m http.server 8029

test:
	node test.mjs
