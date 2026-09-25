# Extracts the .deb and the AppImage and runs what they bundle, offline:
# `euicc-tui --version` and `lpac version`, each inside a namespace
# where /nix is the artifact's own, so a missing store path fails the
# run instead of being found on the build host. Also checks that the
# .deb declares the distribution packages it needs, and that the wrapper
# hands PC/SC to the distribution's client library.
{ pkgs, system }:
pkgs.writeShellApplication {
  name = "linux-artifact-smoke";
  runtimeInputs = [
    pkgs.bubblewrap
    pkgs.coreutils
    pkgs.dpkg
    pkgs.findutils
    pkgs.gnugrep
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
    trap 'rm -rf "$workdir"' EXIT
    prefix="$artifacts_dir/euicc-tui-$artifact_version-${system}"

    fail() {
      echo "linux-artifact-smoke: $*" >&2
      exit 1
    }

    # run a program of an extracted root with only that root's /nix
    in_root() {
      root="$1"
      shift
      bwrap --ro-bind "$root/nix" /nix --proc /proc --dev /dev \
        --tmpfs /tmp --unshare-all --die-with-parent "$@"
    }

    smoke_root() {
      label="$1"
      root="$2"
      exe="$3"
      # lpac must speak the local pcscd's protocol: the wrapper delegates
      # to the distribution's PC/SC client library when there is one
      grep -q 'LIBPCSCLITE_DELEGATE=' "$root$exe" \
        || fail "$label: the wrapper does not delegate to the local PC/SC library"
      version="$(in_root "$root" "$exe" --version)" \
        || fail "$label: euicc-tui --version failed"
      echo "$label: $version"
      grep -qx "euicc-tui ''${artifact_version%%-*}" <<<"$version" \
        || fail "$label: unexpected version '$version'"
      lpac="$(find "$root/nix/store" -maxdepth 3 -path '*-lpac-*/bin/lpac' | head -1)"
      [ -n "$lpac" ] || fail "$label: lpac is not bundled"
      lpac_version="$(in_root "$root" "''${lpac#"$root"}" version)" \
        || fail "$label: lpac version failed"
      echo "$label: lpac $lpac_version"
    }

    deb="$prefix.deb"
    [ -f "$deb" ] || fail "missing $deb"
    depends="$(dpkg-deb -f "$deb" Depends)"
    [ "$depends" = "pcscd, libccid, libpcsclite1" ] \
      || fail "deb: Depends is '$depends', not 'pcscd, libccid, libpcsclite1'"
    echo "deb: Depends: $depends"
    dpkg-deb -x "$deb" "$workdir/deb"
    link="$workdir/deb/usr/bin/euicc-tui"
    [ -L "$link" ] || fail "deb: /usr/bin/euicc-tui is missing"
    smoke_root deb "$workdir/deb" "$(readlink "$link")"

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
