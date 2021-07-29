.PHONY: website
website:
	make -C website

.PHONY: spec
spec:
	make -C spec

.PHONY: images
images:
	make -C infra

.PHONY: toolchain
toolchain:
	cd toolchain && stack build --copy-bins
