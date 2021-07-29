images/%: ../toolchain images.nix
	nix-build -A $(shell basename "$@") -o $@ images.nix
