# The Linux release artifacts: a .deb, an .rpm and an AppImage of the
# wrapped executable, lpac and zbarimg bundled.
#
# The .deb and the .rpm carry the executable's whole closure under
# /nix/store and link /usr/bin/euicc-tui to it, so the bundled lpac is
# the one that runs. They are assembled here with dpkg-deb and rpmbuild,
# not with the NixOS bundlers' toDEB/toRPM, because they have to declare
# what the distribution provides: the PC/SC daemon, the CCID reader
# driver and the PC/SC client library that lpac delegates to (see
# package.nix).
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
  homepage = "https://github.com/lambdasistemi/euicc-tui";
  summary = "terminal UI for eSIM profiles on a removable eUICC card";
  description = ''
    Lists, enables, downloads, nicknames and deletes the eSIM profiles
    on a removable eUICC card through lpac and a PC/SC reader, and sends
    their pending notifications. lpac is bundled.
  '';
  closure = pkgs.closureInfo { rootPaths = [ package ]; };
  # Debian and RPM versions: no hyphen, so a dev suffix is not read as a
  # revision or release
  pkgVersion = builtins.replaceStrings [ "-" ] [ "+" ] artifactVersion;

  # The installed tree shared by both packages. Hard links are not
  # kept: an optimised store links identical files across store paths,
  # and dpkg fails on a link to a file it has not unpacked yet.
  root = pkgs.runCommand "${name}-${artifactVersion}-root" { } ''
    mkdir -p $out/nix/store $out/usr/bin
    while read -r path; do
      cp -a --no-preserve=links "$path" $out/nix/store/
    done < ${closure}/store-paths
    ln -s ${pkgs.lib.getExe package} $out/usr/bin/${name}
  '';

  control = pkgs.writeText "control" ''
    Package: ${name}
    Version: ${pkgVersion}
    Architecture: amd64
    Maintainer: Paolo Veronelli <paolo.veronelli@gmail.com>
    Depends: pcscd, libccid, libpcsclite1
    Section: utils
    Priority: optional
    Homepage: ${homepage}
    Description: ${summary}
    ${pkgs.lib.concatMapStrings (l: " ${l}\n") (
      pkgs.lib.splitString "\n" (pkgs.lib.removeSuffix "\n" description)
    )}'';
  deb = pkgs.runCommand "${name}-${artifactVersion}.deb" { nativeBuildInputs = [ pkgs.dpkg ]; } ''
    cp -a ${root} tree
    chmod -R u+w tree
    mkdir -m 0755 tree/DEBIAN
    install -m 0644 ${control} tree/DEBIAN/control
    dpkg-deb --root-owner-group -Zxz --build tree $out
  '';

  # No automatic requires/provides (they would name the bundled
  # libraries) and no post-install processing (stripping, build-id
  # links) of the store paths. %files is completed at build time with
  # every store path of the closure.
  spec = pkgs.writeText "${name}.spec" ''
    %global __os_install_post %{nil}
    %global _build_id_links none
    %global debug_package %{nil}
    Name: ${name}
    Version: ${pkgVersion}
    Release: 1
    Summary: ${summary}
    License: Apache-2.0
    URL: ${homepage}
    BuildArch: x86_64
    AutoReqProv: no
    Requires: pcsc-lite, pcsc-lite-ccid, pcsc-lite-libs

    %description
    ${description}
    %install
    cp -a ${root}/. %{buildroot}/
    chmod -R u+w %{buildroot}

    %files
    /usr/bin/${name}
  '';
  rpm = pkgs.runCommand "${name}-${artifactVersion}.rpm" { nativeBuildInputs = [ pkgs.rpm ]; } ''
    export HOME=$PWD
    cat ${spec} ${closure}/store-paths > ${name}.spec
    rpmbuild -bb ${name}.spec \
      --define "_topdir $PWD/top" \
      --define "_tmppath $PWD/tmp" \
      --define "_binary_payload w6.xzdio"
    cp top/RPMS/x86_64/*.rpm $out
  '';

  appImage = bundlers.bundlers.${system}.toAppImage package;
in
pkgs.runCommand "${name}-${artifactVersion}-${system}-artifacts"
  {
    passthru = { inherit deb rpm appImage; };
  }
  ''
    mkdir -p $out
    cp -L ${deb} $out/${name}-${artifactVersion}-${system}.deb
    cp -L ${rpm} $out/${name}-${artifactVersion}-${system}.rpm
    cp -L ${appImage} $out/${name}-${artifactVersion}-${system}.AppImage
    cp -L ${appImage} $out/${name}.AppImage
    (cd $out && sha256sum * > SHA256SUMS)
  ''
