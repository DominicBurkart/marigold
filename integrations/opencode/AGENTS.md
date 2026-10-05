# Marigold

This project uses Marigold, a stream-processing language. Source files have the
`.marigold` extension.

After you edit a `.marigold` file, read the diagnostics the language server
reports. Fix every error and every warning (`undefined-stream-variable` is a
warning and is almost always a real mistake). Also read the information
diagnostics: `undefined-fn` and `undefined-struct` are information, because the
name may be a Rust item in scope inside `m!()`. Ignore them only in that case;
otherwise they usually mean a typo, so fix them. Do not treat a result with no
errors as clean while warnings remain.
