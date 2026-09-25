# The released executable, with lpac and zbarimg on its PATH.
#
# The PC/SC client and daemon must speak the same protocol version, and
# a distribution's pcscd may be older than the client library lpac is
# built with. When the distribution ships its own client library, the
# bundled one delegates to it (LIBPCSCLITE_DELEGATE), so lpac always
# speaks the local daemon's protocol; elsewhere (NixOS) the bundled one
# is used. Distributions with pcsc-lite 2.3 or later split the library
# the same way as the bundled one: the delegate must then be their
# libpcsclite_real.so.1, as their libpcsclite.so.1 would delegate back.
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
      } \
      --run '
        if [ -z "''${LIBPCSCLITE_DELEGATE:-}" ]; then
          for lib in \
            /usr/lib/x86_64-linux-gnu/libpcsclite_real.so.1 \
            /usr/lib64/libpcsclite_real.so.1 \
            /usr/lib/libpcsclite_real.so.1 \
            /usr/lib/x86_64-linux-gnu/libpcsclite.so.1 \
            /usr/lib64/libpcsclite.so.1 \
            /usr/lib/libpcsclite.so.1; do
            if [ -e "$lib" ]; then
              export LIBPCSCLITE_DELEGATE="$lib"
              break
            fi
          done
        fi'
  ''
