.PHONY: website
website:
	make -C website

.PHONY: spec
spec:
	make -C spec

.PHONY: images
images:
	make -C infra images/brain-wall-prosecutor
	make -C infra images/brain-wall-web

.PHONY: toolchain
toolchain:
	cd toolchain && stack build --copy-bins

.PHONY: clean
clean:
	make -C website clean
	rm -rf toolchain/.stack-work
