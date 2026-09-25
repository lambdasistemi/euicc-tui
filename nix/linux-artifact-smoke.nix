# Extracts the .deb, the .rpm and the AppImage and checks them offline:
# the distribution packages each declares, the /usr/bin link, the
# wrapper handing PC/SC to the distribution's client library, and
# `euicc-tui --version` and `lpac version` from the extracted files.
# Installing on real distributions is linux-install-test.
{ pkgs, system }:
pkgs.writeShellApplication {
  name = "linux-artifact-smoke";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.cpio
    pkgs.dpkg
    pkgs.findutils
    pkgs.gnugrep
    pkgs.rpm
  ];
  text = ''
    usage() {
      echo "Usage: linux-artifact-smoke --artifacts-dir DIR --artifact-version VERSION" >&2
      exit 2
    }

    artifacts_dir=""
    artifact_version=""
    while [ "$#" -gt 0 ]; do
      case "$1" in
        --artifacts-dir) artifacts_dir="$2"; shift 2 ;;
        --artifact-version) artifact_version="$2"; shift 2 ;;
        *) usage ;;
      esac
    done
    [ -n "$artifacts_dir" ] && [ -n "$artifact_version" ] || usage

    artifacts_dir="$(cd "$artifacts_dir" && pwd)"
    workdir="$(mktemp -d)"
    trap 'chmod -R u+w "$workdir"; rm -rf "$workdir"' EXIT
    prefix="$artifacts_dir/euicc-tui-$artifact_version-${system}"

    fail() {
      echo "linux-artifact-smoke: $*" >&2
      exit 1
    }

    # the executable wrapper inside an extracted root, and what it runs
    smoke_root() {
      label="$1"
      root="$2"
      exe="$root$3"
      [ -x "$exe" ] || fail "$label: $3 is missing"
      # lpac must speak the local pcscd's protocol: the wrapper delegates
      # to the distribution's PC/SC client library when there is one
      grep -q 'LIBPCSCLITE_DELEGATE=' "$exe" \
        || fail "$label: the wrapper does not delegate to the local PC/SC library"
      version="$("$exe" --version)" || fail "$label: euicc-tui --version failed"
      [ "$version" = "euicc-tui ''${artifact_version%%-*}" ] \
        || fail "$label: unexpected version '$version'"
      echo "$label: $version"
      lpac="$(find "$root/nix/store" -maxdepth 3 -path '*-lpac-*/bin/lpac' | head -1)"
      [ -n "$lpac" ] || fail "$label: lpac is not bundled"
      "$lpac" version >/dev/null || fail "$label: lpac version failed"
      echo "$label: lpac bundled"
    }

    usr_bin_target() {
      link="$1/usr/bin/euicc-tui"
      [ -L "$link" ] || fail "$2: /usr/bin/euicc-tui is missing"
      readlink "$link"
    }

    deb="$prefix.deb"
    [ -f "$deb" ] || fail "missing $deb"
    depends="$(dpkg-deb -f "$deb" Depends)"
    [ "$depends" = "pcscd, libccid, libpcsclite1" ] \
      || fail "deb: Depends is '$depends', not 'pcscd, libccid, libpcsclite1'"
    echo "deb: Depends: $depends"
    dpkg-deb -x "$deb" "$workdir/deb"
    smoke_root deb "$workdir/deb" "$(usr_bin_target "$workdir/deb" deb)"

    rpm="$prefix.rpm"
    [ -f "$rpm" ] || fail "missing $rpm"
    requires="$(rpm --dbpath "$workdir/rpmdb" -qp --requires "$rpm" \
      | grep -v '^rpmlib(' | sort | paste -sd, -)"
    [ "$requires" = "pcsc-lite,pcsc-lite-ccid,pcsc-lite-libs" ] \
      || fail "rpm: Requires is '$requires', not 'pcsc-lite,pcsc-lite-ccid,pcsc-lite-libs'"
    echo "rpm: Requires: $requires"
    mkdir -p "$workdir/rpm"
    # no inherited setgid bit on the directories cpio creates
    chmod g-s "$workdir/rpm"
    (cd "$workdir/rpm" && rpm2cpio "$rpm" | cpio -idm --quiet)
    smoke_root rpm "$workdir/rpm" "$(usr_bin_target "$workdir/rpm" rpm)"

    appimage="$prefix.AppImage"
    [ -f "$appimage" ] || fail "missing $appimage"
    mkdir -p "$workdir/appimage"
    cp -L "$appimage" "$workdir/appimage/euicc-tui.AppImage"
    chmod +x "$workdir/appimage/euicc-tui.AppImage"
    (cd "$workdir/appimage" && ./euicc-tui.AppImage --appimage-extract >/dev/null)
    root="$workdir/appimage/squashfs-root"
    smoke_root appimage "$root" "$(readlink "$root/entrypoint")"
  '';
}
