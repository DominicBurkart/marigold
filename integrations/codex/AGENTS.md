# Marigold

This project uses Marigold, a stream-processing language. Source files have the
`.marigold` extension.

After you create or edit any `.marigold` file, call the `marigold_check` tool
from the `marigold` MCP server on that file before you finish. Fix every error
it reports and call it again until it reports no errors. Fix every warning
(the only warning is `undefined-stream-variable` for a variable read before it
is declared, which is almost always a real mistake) even though the result
still says `ok: true`. Also read the information diagnostics:
`undefined-stream-variable` (an undeclared name), `undefined-fn` and
`undefined-struct` mean the name may be a Rust item in scope inside `m!()`, or
for a stream variable a Rust binding with a `get()` method. Ignore them only in
that case; otherwise they usually mean a typo, so fix them. The result reports
`error_count`, `warning_count` and `info_count`.
