{ pkgs ? import <nixpkgs> {} }:

with pkgs;

let
  myghc = haskellPackages.ghcWithPackages (p: with p; [
          QuickCheck
          cryptohash-md5       # for md5 hash (2015, day4)
          base16-bytestring    # for hex conversion (2015, day4)
          regex-posix          # for regex (2015, day5)
          extra                # for groupOn (2017, day1)
          split                # for splitOn (2019, day2)
        ]);

  hss = with haskellPackages; [
    myghc

    cabal-install
    hlint
    ghcid
    hie
    doctest
    hoogle
  ];

  pys = with python3Packages; [
    ptpython

    (python3.withPackages (p: with p; [
      pylint
      flake8
      mypy

      numpy
    ]))
  ];
in
  mkShell {
    name = "aoc-sh";

    buildInputs = hss ++ pys;

    shellHook = ''
      eval "$(egrep ^export "$(type -p ghc)")"
      export PS1="\[\033[1;32m\][$name:\W]\n$ \[\033[0m\]"
    '';
  }
