# Installs the release packages on stock distribution images and checks
# that they work there: the .deb with apt on Ubuntu and Debian, the .rpm
# with dnf on Fedora, and the AppImage's files on Ubuntu with only its
# own /nix. In each, the distribution's pcscd is started and lpac, with
# the environment the euicc-tui wrapper gives it, must be accepted by
# it: pcscd rejects a client speaking another protocol version, which
# lpac would report as "no reader".
#
# Needs a Docker daemon. Files go in with `docker cp`, not bind mounts,
# so a private /tmp (as on the CI runners) is not a problem.
{ pkgs, system }:
let
  # runs inside the container, after the package is installed; the
  # euicc-tui wrapper path is $1
  check = pkgs.writeText "check.sh" ''
    set -eu
    wrapper="$1"
    "$wrapper" --version
    # no polkit daemon in a container; older pcscd has no such option
    polkit=""
    if pcscd --help 2>&1 | grep -q disable-polkit; then polkit=--disable-polkit; fi
    pcscd --foreground --debug $polkit >/tmp/pcscd.log 2>&1 &
    sleep 2
    # the wrapper's environment, then lpac from its PATH, as the app runs it
    out="$(timeout 30 bash -c "$(grep -v '^exec' "$wrapper"); LPAC_APDU=pcsc lpac chip info" 2>&1 || true)"
    echo "lpac: $out"
    if grep -q 'protocol mismatch' /tmp/pcscd.log; then
      echo "pcscd rejected lpac's PC/SC protocol:" >&2
      grep -i protocol /tmp/pcscd.log >&2
      exit 1
    fi
    grep -q 'Client is protocol version' /tmp/pcscd.log || {
      echo "lpac never reached pcscd:" >&2
      tail -5 /tmp/pcscd.log >&2
      exit 1
    }
    grep -i 'Client is protocol version' /tmp/pcscd.log | head -1
    # no reader in a container: SCARD_E_NO_READERS_AVAILABLE
    echo "$out" | grep -q 8010002E || {
      echo "expected lpac to report no reader (8010002E)" >&2
      exit 1
    }
  '';
in
pkgs.writeShellApplication {
  name = "linux-install-test";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.docker-client
  ];
  text = ''
    usage() {
      echo "Usage: linux-install-test --artifacts-dir DIR --artifact-version VERSION" >&2
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
    prefix="$artifacts_dir/euicc-tui-$artifact_version-${system}"

    containers=()
    workdir="$(mktemp -d)"
    cleanup() {
      for c in "''${containers[@]}"; do docker rm -f "$c" >/dev/null 2>&1 || true; done
      chmod -R u+w "$workdir" 2>/dev/null || true
      rm -rf "$workdir"
    }
    trap cleanup EXIT

    # run a script in a fresh container of an image, with files copied in
    # first: in_image IMAGE SCRIPT [SRC DEST]...
    in_image() {
      image="$1"
      script="$2"
      shift 2
      c="$(docker create "$image" bash -c "$script")"
      containers+=("$c")
      docker cp ${check} "$c:/check.sh"
      while [ "$#" -gt 0 ]; do
        docker cp "$1" "$c:$2"
        shift 2
      done
      docker start -a "$c"
    }

    apt_install='
      set -eu
      export DEBIAN_FRONTEND=noninteractive
      apt-get update -qq >/dev/null
      apt-get install -y -qq'
    # the install's output, shown only when it fails
    quiet='>/tmp/install.log 2>&1 || { tail -30 /tmp/install.log; exit 1; }'

    for image in ubuntu:22.04 ubuntu:24.04 debian:12 debian:13; do
      echo "=== .deb on $image"
      in_image "$image" "$apt_install /pkg.deb $quiet
        dpkg -l pcscd libccid libpcsclite1 euicc-tui | grep ^ii
        bash /check.sh \"\$(readlink -f /usr/bin/euicc-tui)\"" \
        "$prefix.deb" /pkg.deb
    done

    for image in fedora:41 fedora:42; do
      echo "=== .rpm on $image"
      in_image "$image" "set -eu
        dnf install -y -q /pkg.rpm $quiet
        rpm -q pcsc-lite pcsc-lite-ccid pcsc-lite-libs euicc-tui
        bash /check.sh \"\$(readlink -f /usr/bin/euicc-tui)\"" \
        "$prefix.rpm" /pkg.rpm
    done

    echo "=== AppImage files on ubuntu:24.04"
    cp -L "$prefix.AppImage" "$workdir/euicc-tui.AppImage"
    chmod +x "$workdir/euicc-tui.AppImage"
    (cd "$workdir" && ./euicc-tui.AppImage --appimage-extract >/dev/null)
    entrypoint="$(readlink "$workdir/squashfs-root/entrypoint")"
    in_image ubuntu:24.04 "$apt_install pcscd libccid libpcsclite1 $quiet
      bash /check.sh $entrypoint" \
      "$workdir/squashfs-root/nix" /nix
  '';
}
