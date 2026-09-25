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
    bundlers = {
      url = "github:NixOS/bundlers";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskellNix,
      bundlers,
      ...
    }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (
      system:
      let
        pkgs = import nixpkgs {
          overlays = [
            haskellNix.overlay
            (_: prev: { zbar = import ./nix/zbar.nix { pkgs = prev; }; })
          ];
          inherit system;
        };
        packageVersion = builtins.head (
          builtins.match ".*\nversion:[[:space:]]*([0-9.]+)\n.*" (builtins.readFile ./euicc-tui.cabal)
        );
        sourceRevision = self.shortRev or (self.dirtyShortRev or "dirty");
        linuxRelease =
          artifactVersion:
          import ./nix/linux-release.nix {
            inherit
              pkgs
              system
              packageVersion
              artifactVersion
              bundlers
              ;
            package = euicc-tui;
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
          linux-release-artifacts = linuxRelease packageVersion;
          linux-dev-release-artifacts = linuxRelease "${packageVersion}-${sourceRevision}";
          linux-artifact-smoke = import ./nix/linux-artifact-smoke.nix { inherit pkgs system; };
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
