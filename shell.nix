# shell.nix : Haskell shell for nix-shell

# Overrideable ghc version by passing compiler env variable
# Example:
# $ nix-shell shell.nix --argstr compiler ghc7103

# To list avaliable ghc version:
# $ nix-env -qaPA nixos.haskell.compiler

{ pkgs ? import <nixpkgs> {}, compiler ? "ghc863" }:

with pkgs;
with haskell.packages.${compiler};

let
  ghc = ghcWithHoogle (p: with p; [
          # split
        ]);
in
mkShell {
  name = "${compiler}-sh";

  # buildInputs = [ ghc hlint ghcid doctest hoogle hie86 ghc-mod86 ];
  buildInputs = [ ghc hlint ghcid doctest hie86 ];

  shellHook = ''
    eval "$(egrep ^export "$(type -p ghc)")"
    export PS1="\[\033[1;32m\][$name:\W]\n$ \[\033[0m\]"
  '';
}
