{
  description = "euicc-tui — terminal UI for eUICC profiles over lpac";
  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    flake-utils.url = "github:hamishmack/flake-utils/hkm/nested-hydraJobs";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          overlays = [ haskellNix.overlay ];
          inherit system;
        };
        project = import ./nix/project.nix { inherit pkgs; };
        components = project.hsPkgs.euicc-tui.components;
        euicc-tui = import ./nix/package.nix {
          inherit pkgs;
          exe = components.exes.euicc-tui;
        };
        checks = import ./nix/checks.nix {
          inherit pkgs components;
          src = ./.;
        };
      in
      {
        packages = {
          default = euicc-tui;
          inherit euicc-tui;
          unit-tests = components.tests.unit-tests;
        };
        checks = builtins.removeAttrs checks [ "apps" ] // {
          inherit euicc-tui;
        };
        apps = import ./nix/apps.nix { inherit pkgs checks; } // {
          default = {
            type = "app";
            program = pkgs.lib.getExe euicc-tui;
          };
        };
        devShells.default = project.shell;
      }
    );
}
