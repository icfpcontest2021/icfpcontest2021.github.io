# This builds docker images we can run on AWS easily using nix.
{ }:
let
  # Pin all packages to an exact version.
  pkgs = import (fetchTarball https://github.com/NixOS/nixpkgs/archive/593f8bde3c97f5202d762d8195c295ebc03665da.tar.gz) { };

  # Override some dependency versions and flags in the above.
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: rec {
      Spock = pkgs.haskell.lib.dontCheck super.Spock;
      tmp-postgres = pkgs.haskell.lib.dontCheck super.tmp-postgres;
      mwc-random = pkgs.haskell.lib.dontCheck (self.callCabal2nix
        "mwc-random"
        (builtins.fetchTarball {
          url = "https://hackage.haskell.org/package/mwc-random-0.15.0.1/mwc-random-0.15.0.1.tar.gz";
        })
        { });
    };
  };

  gitVersion = pkgs.lib.substring 0 8 (pkgs.lib.commitIdFromGitRepo ./../.git);

  brainWall =
    pkgs.haskell.lib.overrideCabal
      (pkgs.haskell.lib.addBuildDepend
        (pkgs.haskell.lib.addTestToolDepend
          (haskellPackages.callCabal2nix "brain-wall" ./../toolchain { })
          pkgs.postgresql)
        pkgs.git)
      (drv: {
        postPatch = drv.postPatch or "" + ''
          sed \
            -i.bak \
            '/^version =/,$cversion = "${gitVersion}"' \
            lib/BrainWall/Version.hs
        '';
      });

  brainWallExes = pkgs.haskell.lib.justStaticExecutables brainWall;

  alpine = pkgs.dockerTools.pullImage {
    imageName = "alpine";
    imageDigest = "sha256:69e70a79f2d41ab5d637de98c1e0b055206ba40a8145e7bddb55ccc04e13cf8f";
    sha256 = "13p8046wx8isw4pi57xaxgrn11d92ql30p0lss090wcaliv7rjmc";
  };
in
{
  brain-wall-web = pkgs.dockerTools.buildImage {
    name = "brain-wall-web";
    fromImage = alpine;
    config = {
      ExposedPorts = { "3000" = { }; };
      Cmd = [ "${brainWallExes}/bin/brain-wall-web" ];
    };
  };
  brain-wall-prosecutor = pkgs.dockerTools.buildImage {
    name = "brain-wall-prosecutor";
    fromImage = alpine;
    config = {
      Cmd = [ "${brainWallExes}/bin/brain-wall-prosecutor" ];
    };
  };
}
