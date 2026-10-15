# Marigold

This project uses Marigold, a stream-processing language. Source files have the
`.marigold` extension.

After you edit a `.marigold` file, read the diagnostics the language server
reports. Fix every error and every warning (the only warning is
`undefined-stream-variable` for a variable read before it is declared, which is
almost always a real mistake). Also read the information diagnostics:
`undefined-stream-variable` for an undeclared name, `undefined-fn` and
`undefined-struct` are information, because the name may be a Rust item in
scope inside `m!()` (or a Rust binding with a `get()` method for a stream
variable). Ignore them only in that case;
otherwise they usually mean a typo, so fix them. Do not treat a result with no
errors as clean while warnings remain.
