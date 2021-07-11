UPDATES=$(shell find updates -type f)

build: \
	_site/about.html \
	_site/faq.html \
	_site/feed.xml \
	_site/index.html \
	_site/logo.svg \
	_site/prizes.html \
	_site/rules.html \
	_site/specification.html \
	_site/spec-v1.0.pdf \
	_site/spec-v1.1.pdf \
	_site/spec-v1.2.pdf \
	_site/spec-v2.0.pdf \
	_site/spec-v2.1.pdf \
	_site/spec-v3.0.pdf \
	_site/spec-v4.0.pdf \
	_site/spec-v4.1.pdf \
	_site/style.css

_site:
	mkdir _site

_site/logo.svg: | _site
	cp logo.svg $@

_site/%.pdf: %.pdf | _site
	cp $< $@

_site/style.css: style.css | _site
	cp style.css $@

_site/%.html: %.html template.sh | _site
	./template.sh $< >$@

_site/feed.xml: feed.sh $(UPDATES) | _site
	./feed.sh >$@

index.html: index.sh $(UPDATES)
	./index.sh >$@

clean:
	rm -rf _site
	rm index.html

.DELETE_ON_ERROR:

.PHONY: clean
