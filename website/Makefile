UPDATES=$(shell find updates -type f)

build: \
	_site/about.html \
	_site/assets/animation-01.svg \
	_site/assets/contest-presentation.jpg \
	_site/assets/contest-presentation.mp4 \
	_site/assets/contest-presentation.vtt \
	_site/assets/logo.svg \
	_site/assets/style.css \
	_site/faq.html \
	_site/feed.xml \
	_site/index.html \
	_site/prizes.html \
	_site/rules.html \
	_site/scoreboard.html \
	_site/scoreboard-lightning.html \
	_site/specification.html \
	_site/spec-v1.0.pdf \
	_site/spec-v1.1.pdf \
	_site/spec-v1.2.pdf \
	_site/spec-v2.0.pdf \
	_site/spec-v2.1.pdf \
	_site/spec-v3.0.pdf \
	_site/spec-v4.0.pdf \
	_site/spec-v4.1.pdf \
	_site/writeups.html

_site:
	mkdir _site

_site/assets: | _site
	mkdir _site/assets

_site/assets/%: assets/% | _site/assets
	cp $< $@

_site/%.pdf: spec/%.pdf | _site
	cp $< $@

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
