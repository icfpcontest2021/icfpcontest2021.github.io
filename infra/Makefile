default: \
	images/brain-wall-prosecutor \
	images/brain-wall-web

images/%:
	nix-build -A $(shell basename "$@") -o $@ images.nix
