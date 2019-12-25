## default.nix
## NOTE: There are two ways to build the site
##       1. use nix-build (outside nix-shell) to build site
##          (need to remove `./.ghc.environment.*` first)
##       2. use `cabal new-build` within `nix-shell`

{
pkgs ? import <nixpkgs> {},
compiler ? "ghc865",
}:

with pkgs;

let
  haskellPackages = haskell.packages.${compiler};
  drv = haskellPackages.callCabal2nix "AOC19" ./. {};

  ghcides = import (fetchTarball "https://github.com/hercules-ci/ghcide-nix/tarball/master") {};
  ghcide = ghcides."ghcide-${compiler}";

  hsPcks = with haskellPackages;
         [
           cabal-install
           doctest
           ghcid
           ghcide
           hasktags
           hie
           hie-bios
           hlint
           hoogle
           hpack

           nvim_hs
         ];

  pyPcks = with python3Packages;
         [
           pylint
           flake8
           mypy

           numpy
         ];

  dev = drv.env.overrideAttrs(attr: {
    buildInputs = attr.buildInputs
               ++ hsPcks
               ++ pyPcks;

    name = "aoc-sh";
    shellHook = ''
      eval "$(egrep ^export "$(type -p ghc)")"
      export PS1="\[\033[1;32m\][$name:\W]\n$ \[\033[0m\]"
    '';
  });

in
  if lib.inNixShell then dev else drv
