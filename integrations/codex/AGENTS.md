# Marigold

This project uses Marigold, a stream-processing language. Source files have the
`.marigold` extension.

After you create or edit any `.marigold` file, call the `marigold_check` tool
from the `marigold` MCP server on that file before you finish. Fix every error
it reports and call it again until it reports no errors. Also read the
warnings: an undefined function, stream variable or struct is reported as a
warning and the result still says `ok: true`, but it usually means a typo, so
fix it unless the name is defined in Rust code outside the program.
