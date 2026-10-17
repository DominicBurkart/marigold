# Marigold

This project uses Marigold, a stream-processing language. Source files have the
`.marigold` extension.

After you edit a `.marigold` file, opencode reports only error diagnostics. It
does not show warnings or information, so a clean result does not mean the file
is clean. Before finishing, re-read the file and check by hand for: variables
read before they are declared, and undeclared function or struct names (these
may be Rust items in scope inside `m!()`; otherwise they are usually typos).
