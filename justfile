# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Format Haskell, cabal and nix files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    for i in {1..3}; do
        fourmolu -i src app test
    done
    cabal-fmt -i euicc-tui.cabal
    nixfmt flake.nix nix/*.nix

# Check formatting without changing files
format-check:
    #!/usr/bin/env bash
    set -euo pipefail
    fourmolu -m check src app test
    diff -u euicc-tui.cabal <(cabal-fmt euicc-tui.cabal)
    nixfmt --check flake.nix nix/*.nix

# Run hlint
hlint:
    hlint src app test

# Build all components
build:
    cabal build all --enable-tests -O0

# Run unit tests, optionally matching a pattern
unit match="":
    #!/usr/bin/env bash
    set -euo pipefail
    if [[ '{{ match }}' == "" ]]; then
        cabal test unit-tests -O0 --test-show-details=direct
    else
        cabal test unit-tests -O0 \
            --test-show-details=direct \
            --test-option=--match \
            --test-option="{{ match }}"
    fi

# Check the package for Hackage
cabal-check:
    cabal check

# Everything CI checks, from inside the dev shell
ci:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    just format-check
    just hlint
    just cabal-check

# Run the TUI against the card in the reader
run:
    cabal run euicc-tui -O0
