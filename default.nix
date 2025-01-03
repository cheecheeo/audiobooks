# default.nix
# https://srid.ca/haskell-nix
let
  pkgs = import <nixpkgs> { };
in
  pkgs.haskellPackages.developPackage {
    root = ./.;
    modifier = drv:
      pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages;
        [ cabal-install
          cabal-fmt
          doctest
          ghcid
          pkgs.ffmpeg-full
          pkgs.yt-dlp
        ]);
  }
