# shell.nix : Haskell shell for nix-shell

# Overrideable ghc version by passing compiler env variable
# Example:
# $ nix-shell shell.nix --argstr compiler ghc7103

# To list avaliable ghc version:
# $ nix-env -qaPA nixos.haskell.compiler

{ pkgs ? import <nixpkgs> {}, compiler ? "ghc862" }:

with pkgs;
with haskell.packages.${compiler};

let
  ghc = ghcWithPackages (p: with p; [
          split
          time_1_9_2
          # hspec
          # fgl
        ]);
in
mkShell {
  name = "${compiler}-sh";

  buildInputs = [ ghc hlint ghcid doctest ];

  shellHook = ''
    eval "$(egrep ^export "$(type -p ghc)")"
    export PS1="\[\033[1;32m\][$name:\W]\n$ \[\033[0m\]"
  '';
}
