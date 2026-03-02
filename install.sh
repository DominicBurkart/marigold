#!/bin/sh
set -eu

main() {
    if ! command -v cargo >/dev/null 2>&1; then
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        . "$HOME/.cargo/env"
    fi

    cargo install marigold -F cli

    echo "marigold $(marigold --version) installed successfully"
}

main
