# The Linux release artifacts: a .deb and an AppImage of the wrapped
# executable, lpac and zbarimg bundled.
#
# The .deb carries the executable's whole closure under /nix/store and
# links /usr/bin/euicc-tui to it, so the bundled lpac is the one that
# runs. It is assembled here with dpkg-deb, not with the NixOS bundlers'
# toDEB, because it has to declare what the distribution provides: the
# PC/SC daemon, the CCID reader driver and the PC/SC client library
# that lpac delegates to (see package.nix).
{
  pkgs,
  system,
  packageVersion,
  artifactVersion ? packageVersion,
  package,
  bundlers,
}:
let
  name = "euicc-tui";
  closure = pkgs.closureInfo { rootPaths = [ package ]; };
  # a Debian version: no hyphen, so a dev suffix is not read as a revision
  debVersion = builtins.replaceStrings [ "-" ] [ "+" ] artifactVersion;
  control = pkgs.writeText "control" ''
    Package: ${name}
    Version: ${debVersion}
    Architecture: amd64
    Maintainer: Paolo Veronelli <paolo.veronelli@gmail.com>
    Depends: pcscd, libccid, libpcsclite1
    Section: utils
    Priority: optional
    Homepage: https://github.com/lambdasistemi/euicc-tui
    Description: terminal UI for eSIM profiles on a removable eUICC card
     Lists, enables, downloads, nicknames and deletes the eSIM profiles
     on a removable eUICC card through lpac and a PC/SC reader, and sends
     their pending notifications. lpac is bundled.
  '';
  deb =
    pkgs.runCommand "${name}-${artifactVersion}.deb"
      {
        nativeBuildInputs = [ pkgs.dpkg ];
      }
      ''
        root=$PWD/root
        mkdir -p $root/nix/store $root/usr/bin $root/DEBIAN
        while read -r path; do
          cp -a "$path" $root/nix/store/
        done < ${closure}/store-paths
        chmod -R u+w $root/nix
        ln -s ${pkgs.lib.getExe package} $root/usr/bin/${name}
        cp ${control} $root/DEBIAN/control
        chmod 0755 $root/DEBIAN
        chmod 0644 $root/DEBIAN/control
        dpkg-deb --root-owner-group -Zxz --build $root $out
      '';
  appImage = bundlers.bundlers.${system}.toAppImage package;
in
pkgs.runCommand "${name}-${artifactVersion}-${system}-artifacts"
  {
    passthru = { inherit deb appImage; };
  }
  ''
    mkdir -p $out
    cp -L ${deb} $out/${name}-${artifactVersion}-${system}.deb
    cp -L ${appImage} $out/${name}-${artifactVersion}-${system}.AppImage
    cp -L ${appImage} $out/${name}.AppImage
    (cd $out && sha256sum * > SHA256SUMS)
  ''
