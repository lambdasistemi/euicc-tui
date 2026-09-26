# euicc-tui

A terminal UI for a removable eUICC (eSIM) card sitting in a PC/SC
smart-card reader. It drives [lpac](https://github.com/estkme-group/lpac)
so profiles can be inspected and switched without remembering ICCIDs.

- **Profiles**: name or nickname, provider, ICCID and state, with the
  card's EID and free memory in the header.
- **Enable** the selected profile after a y/n confirmation. The
  previously enabled profile is disabled by the card.
- **Nickname** the selected profile, so several plans from the same
  provider stay distinguishable in the list.
- **Notifications**: list the pending ones and send the selected one
  or all of them.
- **Download** a profile from an SM-DP+ address and activation code.
  A full `LPA:1$<smdp>$<code>` string can be pasted into either
  field, or picked as a **QR image file** (a screenshot or the image
  saved from the purchase email) with a cursor-driven picker over
  the filesystem: Enter on the empty QR path field opens it,
  Enter picks the image and the decode starts. The code is
  masked on screen.
- **Guided install** (`g`): pick the purchase QR image, check what it
  holds, press enter, and land back on the list with the new plan
  selected (`e` enables it, `m` names it). Activation codes that ask
  for a confirmation code (GSMA `LPA:1$...$...$1`) are handled here:
  the code is asked for, masked, and passed to `lpac -c`.
- **Refresh** the card state at any time.
- **Delete** (`D`) a disabled profile, guarded: the enabled profile is
  refused, and the delete runs only after the last four digits of the
  ICCID are typed back. Deleting is permanent; the purchase QR usually
  cannot install the plan again. Send the resulting notification
  (`n`, `a`) while online.

## Install and run

Plug the reader in, put the card in, and run `euicc-tui` from a
terminal in the machine's own desktop session: `pcscd` admits only
the active local session (see `8010006A` under
[Error messages](#error-messages)).

### Debian and Ubuntu (amd64)

Download `euicc-tui-<version>-x86_64-linux.deb` from the
[latest release](https://github.com/lambdasistemi/euicc-tui/releases/latest)
and install it with `apt`, which also installs `pcscd`, the CCID
reader driver (`libccid`) and the PC/SC client library:

```sh
sudo apt install ./euicc-tui-<version>-x86_64-linux.deb
euicc-tui
```

`lpac` and `zbar` (QR decoding) are bundled; the distribution's own
`lpac`, if any, is never used.

### Fedora (x86_64)

Download `euicc-tui-<version>-x86_64-linux.rpm` from the same release
and install it with `dnf`, which also installs `pcsc-lite`, the CCID
driver (`pcsc-lite-ccid`) and the PC/SC client library
(`pcsc-lite-libs`); then start the daemon:

```sh
sudo dnf install ./euicc-tui-<version>-x86_64-linux.rpm
sudo systemctl enable --now pcscd.socket
euicc-tui
```

### Other distributions (x86_64)

Install `pcscd` and the CCID driver (`ccid` or `libccid`) from the
distribution and start `pcscd` (usually `systemctl enable --now
pcscd.socket`), then run the AppImage from the same release:

```sh
curl -LO https://github.com/lambdasistemi/euicc-tui/releases/latest/download/euicc-tui.AppImage
chmod +x euicc-tui.AppImage
./euicc-tui.AppImage
```

The AppImage mounts its bundled files in a user namespace; Ubuntu
24.04 and later restrict those for unprivileged programs, so use the
`.deb` there.

### Nix

With flakes enabled, on a machine where `pcscd` is running:

```sh
nix run github:lambdasistemi/euicc-tui
```

On NixOS, `services.pcscd.enable = true;` provides the daemon.

### What the packages bundle

Every package wraps `lpac` and `zbar` into the program's `PATH`, and
every `lpac` call runs with `LPAC_APDU=pcsc`, whatever the caller's
environment says. The PC/SC client and `pcscd` must speak the same
protocol version, so when the distribution ships its own client
library (`libpcsclite_real.so.1`, or `libpcsclite.so.1` before
pcsc-lite 2.3), the bundled one hands over to it
(`LIBPCSCLITE_DELEGATE`); on NixOS the bundled one is used.

## Keys

Press `?` for the keys of the current screen.

The colours follow the terminal's background, light or dark, as the
terminal reports it at start. Terminals that report light/dark changes
(DEC mode 2031, e.g. Ghostty) are followed live; `t` swaps by hand in
any terminal. `EUICC_TUI_THEME=light` or `dark` fixes the theme.

The mouse works too: a click selects a row, a tab or a picker entry;
a click on the selected profile asks to enable it, and on the selected
picker entry picks it. A click on a key hint, such as `y enable` or
`esc cancel` in a dialog, presses that key, and a click on a form field
focuses it. The wheel moves the selection. Notifications are sent only
from the keyboard, and deleting still needs the ICCID digits typed.
With mouse mode on, most terminals select text with shift+drag.

| View | Keys |
|---|---|
| Profiles | up/down (or j/k) select, `e`/enter enable, `m` nickname, `n` notifications, `d` download, `g` guided install, `D` delete, `r` refresh, `t` light/dark, `q` quit |
| Delete | type the last four ICCID digits, enter deletes if they match (anything else cancels), esc cancel |
| Confirmation | `y` enable, `n`/esc cancel |
| Nickname | type, enter set, esc cancel |
| Notifications | up/down select, `s` send selected, `a` send all, `p`/esc back, `r` refresh, `q` quit |
| Download | tab switch field, enter on an empty QR path opens the file picker, enter on the code downloads, esc cancel (clears the code) |
| File picker | up/down (or j/k) select, enter picks the file or enters a directory, backspace parent, esc cancel |
| Guided install | as download; after the QR is read, enter installs and esc cancels; a confirmation code is typed and submitted with enter |

While an `lpac` call runs, the status line says so and new actions are
refused; the screen keeps responding.

## Safety rules

- **Delete is guarded, disable does not exist.** Only a disabled
  profile can be deleted, and only after its ICCID's last four digits
  are typed back; a wrong guess cancels. No key disables a profile:
  the set of `lpac` commands the program can issue does not contain
  it.
- **Switching works offline.** Enabling a profile is a card-local
  operation; no network is needed. It can be checked with
  `unshare -rn euicc-tui`.
- **Notifications need the network.** Enabling, disabling and
  downloading leave notifications on the card for the operators'
  servers. Sending them contacts those servers; a failure is reported
  on the status line and the notification stays pending. A
  notification the server accepted is removed from the card
  (`lpac notification process -r`).
- **The activation code stays out of sight.** It is masked in the
  form, cleared after submission or cancel, and removed from any error
  text before it is shown. The confirmation code of a guided install
  gets the same treatment. Both are passed to `lpac` as command-line
  arguments, so they are visible in the process table for the
  duration of the download.

## Error messages

Known PC/SC failures are translated:

| lpac says | Meaning |
|---|---|
| `8010002E` | No reader visible to `pcscd`: plug the reader in and check that `pcscd` is running. |
| `8010000C`, `80100069` | Reader present, no card. |
| `8010006A` | `pcscd`'s polkit policy only admits the active local session: run at the machine's own desk, or via `sudo`. |
| `LIBUSB_ERROR_ACCESS` | The reader was plugged in before its udev rules applied: re-plug it. |
| `euicc_init` on `/dev/cdc-wdm0` | `lpac` used its modem backend; it must run with `LPAC_APDU=pcsc`. |

## Development

```sh
nix develop
just ci          # build, unit tests, format check, hlint, cabal check
nix flake check  # the same tests, sandboxed
just run         # start the TUI from the working tree
```

The pure parts (lpac output decoding, failure classification,
activation codes, QR decoding, the UI state machine) are unit-tested
against recorded `lpac` output and synthetic QR images (built with
`qrencode`, never real codes) in `test/fixtures`. Only the process
runner and the terminal drawing are untested.
