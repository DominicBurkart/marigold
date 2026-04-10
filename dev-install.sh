#!/bin/sh
# Dev environment setup for marigold contributors and agents.
# Run from the repo root: sh dev-install.sh
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

# 1. Rust toolchain -----------------------------------------------------------
if ! command -v cargo >/dev/null 2>&1; then
    echo "Rust/Cargo not found. Installing via rustup..."
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y
    # shellcheck source=/dev/null
    . "$HOME/.cargo/env"
else
    CURRENT_VERSION=$(rustc --version | cut -d' ' -f2)
    if ! version_ge "$CURRENT_VERSION" "$MSRV"; then
        echo "Rust $CURRENT_VERSION is older than required ($MSRV). Updating..."
        rustup update stable
    fi
fi

# Ensure rustfmt and clippy are present (as declared in rust-toolchain.toml)
rustup component add rustfmt clippy

# 2. Cargo dev tools ----------------------------------------------------------
echo "Installing cargo-audit..."
cargo install cargo-audit --quiet

echo "Installing cargo-deny..."
cargo install cargo-deny --quiet

# 3. Validate build -----------------------------------------------------------
echo "Building workspace (all features)..."
cargo build --all-features

echo ""
echo "Dev environment ready. Common commands:"
echo "  cargo test --all-features          run all tests"
echo "  cargo fmt --all -- --check         check formatting"
echo "  cargo clippy --all-features        lint"
echo "  cargo audit                        security audit"
echo "  cargo deny check                   licence compliance"
