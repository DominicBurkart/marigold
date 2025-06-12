use marigold::m;
use marigold::marigold_impl::StreamExt;

use color_palette_picker::compare_contrast;

/// Uses a multithread tokio runtime and tokio-console.
#[cfg(feature = "tokio")]
#[tokio::main]
async fn main() {
    console_subscriber::init();
    // TODO: Uncomment when permutations_with_replacement is implemented
    // println!(
    //     "program complete. Best colors: {:?}",
    //     m!(
    //         range(0, 255)
    //         .permutations_with_replacement(3)
    //         .combinations(5)
    //         .keep_first_n(20, compare_contrast)
    //         .return
    //     )
    //     .await
    //     .collect::<Vec<_>>()
    //     .await
    // );
}

/// Uses a multithread async-std runtime.
#[cfg(feature = "async-std")]
#[async_std::main]
async fn main() {
    // TODO: Uncomment when permutations_with_replacement is implemented
    // println!(
    //     "program complete. Best colors: {:?}",
    //     m!(
    //         range(0, 255)
    //         .permutations_with_replacement(3)
    //         .combinations(5)
    //         .keep_first_n(20, compare_contrast)
    //         .return
    //     )
    //     .await
    //     .collect::<Vec<_>>()
    //     .await
    // );
}
/// Returns a single future, where all computation occurs in a single thread.
/// This allows Marigold programs to compile with a WASM target.
#[cfg(not(any(feature = "tokio", feature = "async-std")))]
#[tokio::main(flavor = "current_thread")]
async fn main() {
    // TODO: Uncomment when permutations_with_replacement is implemented
    // println!(
    //     "program complete. Best colors: {:?}",
    //     m!(
    //         range(0, 255)
    //         .permutations_with_replacement(3)
    //         .combinations(5)
    //         .keep_first_n(20, compare_contrast)
    //         .return
    //     )
    //     .await
    //     .collect::<Vec<_>>()
    //     .await
    // );
}

#[cfg(test)]
mod tests {
    use super::*;

    // TODO: Uncomment when permutations_with_replacement is implemented
    // #[tokio::test]
    // async fn limited_palette() {
    //     let mod_fifty = |i: &u8| i % 50 == 0; // decrease space to search
    //     assert_eq!(
    //         m!(
    //             range(0, 255)
    //             .filter(mod_fifty)
    //             .permutations_with_replacement(3)
    //             .combinations(2)
    //             .keep_first_n(2, compare_contrast)
    //             .return
    //         )
    //         .await
    //         .collect::<Vec<_>>()
    //         .await,
    //         vec![
    //             vec![vec![0, 0, 0], vec![250, 250, 250]],
    //             vec![vec![0, 0, 50], vec![250, 250, 250]]
    //         ]
    //     );
    // }
}
