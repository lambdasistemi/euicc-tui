# Verification steps. Each is a writeShellApplication (exposed as an
# app for `nix run .#<name>`) and a runCommand check that invokes the
# same app inside the sandbox (for `nix flake check`).
{
  pkgs,
  components,
  src,
}:
let
  scripts = {
    unit = {
      runtimeInputs = [ components.tests.unit-tests pkgs.zbar ];
      text = ''
        unit-tests
      '';
    };
  };

  mkApp =
    name:
    { runtimeInputs, text }:
    pkgs.writeShellApplication { inherit name text runtimeInputs; };

  mkCheck =
    name: spec:
    let
      app = mkApp name spec;
    in
    pkgs.runCommand name
      {
        nativeBuildInputs = [ pkgs.glibcLocales ];
        LANG = "C.UTF-8";
        LC_ALL = "C.UTF-8";
      }
      ''
        set -euo pipefail
        cd ${src}
        ${pkgs.lib.getExe app}
        touch $out
      '';

  apps = builtins.mapAttrs mkApp scripts;
in
builtins.mapAttrs mkCheck scripts
// {
  library = components.library;
  exe = components.exes.euicc-tui;
  inherit apps;
}
