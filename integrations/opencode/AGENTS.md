# Marigold

This project uses Marigold, a stream-processing language. Source files have the
`.marigold` extension.

After you edit a `.marigold` file, read the diagnostics the language server
reports. Fix every error. Also read the warnings: an undefined function,
stream variable or struct (`undefined-fn`, `undefined-stream-variable`,
`undefined-struct`) is reported as a warning, not an error, but it usually
means a typo, so fix it unless the name is defined in Rust code outside the
program. Do not treat a result with no errors as clean while warnings remain.
