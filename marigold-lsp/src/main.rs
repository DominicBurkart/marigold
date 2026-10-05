#![forbid(unsafe_code)]

use lsp_server::Connection;

fn main() -> Result<(), marigold_lsp::Error> {
    let (connection, io_threads) = Connection::stdio();
    marigold_lsp::serve(&connection)?;
    drop(connection);
    io_threads.join()?;
    Ok(())
}
