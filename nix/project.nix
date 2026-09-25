{ pkgs }:
let
  indexState = "2026-09-01T00:00:00Z";
in
pkgs.haskell-nix.cabalProject' {
  name = "euicc-tui";
  src = ../.;
  compiler-nix-name = "ghc9123";
  index-state = indexState;
  shell = {
    tools = {
      cabal = {
        index-state = indexState;
      };
      fourmolu = {
        index-state = indexState;
      };
      hlint = {
        index-state = indexState;
      };
      haskell-language-server = {
        index-state = indexState;
      };
    };
    buildInputs = [
      pkgs.just
      pkgs.nixfmt
      pkgs.lpac
      pkgs.zbar
      pkgs.haskellPackages.cabal-fmt
    ];
  };
}
