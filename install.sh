#!/bin/sh
set -eu

MSRV="1.70"

version_ge() {
    v1=$(printf '%s' "$1" | cut -d'-' -f1)
    v2=$(printf '%s' "$2" | cut -d'-' -f1)
    maj1=$(printf '%s' "$v1" | cut -d'.' -f1)
    min1=$(printf '%s' "$v1" | cut -d'.' -f2)
    pat1=$(printf '%s' "$v1" | cut -d'.' -f3)
    maj2=$(printf '%s' "$v2" | cut -d'.' -f1)
    min2=$(printf '%s' "$v2" | cut -d'.' -f2)
    pat2=$(printf '%s' "$v2" | cut -d'.' -f3)
    maj1=${maj1:-0}; min1=${min1:-0}; pat1=${pat1:-0}
    maj2=${maj2:-0}; min2=${min2:-0}; pat2=${pat2:-0}
    if [ "$maj1" -gt "$maj2" ]; then return 0; fi
    if [ "$maj1" -lt "$maj2" ]; then return 1; fi
    if [ "$min1" -gt "$min2" ]; then return 0; fi
    if [ "$min1" -lt "$min2" ]; then return 1; fi
    [ "$pat1" -ge "$pat2" ]
}

main() {
    if ! command -v cargo >/dev/null 2>&1; then
        echo "Rust/Cargo not found. Installing via rustup..."
        curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
        # shellcheck source=/dev/null
        . "$HOME/.cargo/env"
    else
        CURRENT_VERSION=$(rustc --version | cut -d' ' -f2 | cut -d'-' -f1)
        if ! version_ge "$CURRENT_VERSION" "$MSRV"; then
            echo ""
            echo "Error: Rust $CURRENT_VERSION is older than the minimum required version $MSRV."
            echo "To update Rust, run:"
            echo "  rustup update stable"
            echo ""
            echo "If rustup is not installed, reinstall Rust from https://rustup.rs"
            echo ""
            exit 1
        fi
    fi

    cargo install marigold -F cli

    echo "marigold $(marigold --version) installed successfully"
}

main
