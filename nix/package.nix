# The released executable, with lpac on its PATH.
{ pkgs, exe }:
pkgs.runCommand "euicc-tui"
  {
    nativeBuildInputs = [ pkgs.makeWrapper ];
    meta.mainProgram = "euicc-tui";
  }
  ''
    mkdir -p $out/bin
    makeWrapper ${exe}/bin/euicc-tui $out/bin/euicc-tui \
      --prefix PATH : ${
        pkgs.lib.makeBinPath [
          pkgs.lpac
          pkgs.zbar
        ]
      }
  ''
