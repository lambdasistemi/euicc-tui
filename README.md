# euicc-tui

A terminal UI for a removable eUICC (eSIM) card sitting in a PC/SC
smart-card reader. It drives [lpac](https://github.com/estkme-group/lpac)
so profiles can be inspected and switched without remembering ICCIDs.

- **Profiles**: name or nickname, provider, ICCID and state, with the
  card's EID and free memory in the header.
- **Enable** the selected profile after a y/n confirmation. The
  previously enabled profile is disabled by the card.
- **Notifications**: list the pending ones and send the selected one
  or all of them.
- **Download** a profile from an SM-DP+ address and activation code.
  A full `LPA:1$<smdp>$<code>` string can be pasted into either
  field. The code is masked on screen.
- **Refresh** the card state at any time.

There is **no delete action**. Profiles can only be removed with
another tool.

## Install and run

With Nix (flakes enabled) on a machine where `pcscd` is running:

```sh
nix run github:lambdasistemi/euicc-tui
```

The package wraps `lpac` into the program's `PATH`, and every `lpac`
call runs with `LPAC_APDU=pcsc`, whatever the caller's environment
says.

On NixOS, `services.pcscd.enable = true;` provides the daemon.

## Keys

| View | Keys |
|---|---|
| Profiles | up/down (or j/k) select, `e`/enter enable, `n` notifications, `d` download, `r` refresh, `q` quit |
| Confirmation | `y` enable, `n`/esc cancel |
| Notifications | up/down select, `s` send selected, `a` send all, `p`/esc back, `r` refresh, `q` quit |
| Download | tab switch field, enter download, esc cancel (clears the code) |

While an `lpac` call runs, the status line says so and new actions are
refused; the screen keeps responding.

## Safety rules

- **No delete.** The program has no way to delete or disable a
  profile: the set of `lpac` commands it can issue does not contain
  them.
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
  text before it is shown. It is passed to `lpac` as a command-line
  argument, so it is visible in the process table for the duration of
  the download.

## Error messages

Known PC/SC failures are translated:

| lpac says | Meaning |
|---|---|
| `8010002E` | No reader visible to `pcscd`. |
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
activation codes, the UI state machine) are unit-tested against
recorded `lpac` output in `test/fixtures`. Only the process runner and
the terminal drawing are untested.
