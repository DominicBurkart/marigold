#!/bin/sh
set -e

MSRV=$(grep '^MSRV=' install.sh | cut -d'"' -f2)
MSRV_MAJOR=$(printf '%s' "$MSRV" | cut -d'.' -f1)
MSRV_MINOR=$(printf '%s' "$MSRV" | cut -d'.' -f2)
OLD_MINOR=$((MSRV_MINOR - 1))
OLD_VERSION="${MSRV_MAJOR}.${OLD_MINOR}.0"

TMPBIN=$(mktemp -d)
trap 'rm -rf "$TMPBIN"' EXIT

cat > "$TMPBIN/rustc" << MOCKEOF
#!/bin/sh
echo "rustc $OLD_VERSION (mockhash 2020-01-01)"
MOCKEOF
chmod +x "$TMPBIN/rustc"

cat > "$TMPBIN/cargo" << MOCKEOF
#!/bin/sh
echo "cargo $OLD_VERSION (mockhash 2020-01-01)"
MOCKEOF
chmod +x "$TMPBIN/cargo"

OUTPUT=$(PATH="$TMPBIN:$PATH" sh install.sh 2>&1) || EXIT_CODE=$?
EXIT_CODE=${EXIT_CODE:-0}

if [ "$EXIT_CODE" -ne 1 ]; then
    echo "FAIL: Expected exit code 1, got $EXIT_CODE"
    exit 1
fi

if echo "$OUTPUT" | grep -q "minimum required version $MSRV"; then
    echo "PASS: MSRV check correctly rejects Rust $OLD_VERSION (requires $MSRV)"
else
    echo "FAIL: Expected error message mentioning 'minimum required version $MSRV'"
    echo "Output was:"
    echo "$OUTPUT"
    exit 1
fi

if echo "$OUTPUT" | grep -qi "reinstall\|update\|rustup"; then
    echo "PASS: Reinstall guidance present"
else
    echo "FAIL: Expected reinstall/update guidance in output"
    echo "Output was:"
    echo "$OUTPUT"
    exit 1
fi
