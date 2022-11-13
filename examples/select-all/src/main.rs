use marigold::m;
use marigold::marigold_impl::StreamExt;
use std::collections::HashSet;

async fn run() {
    let ints_with_duplicates = m!(
        select_all(
            range(0, 10),
            range(10, 20),
            range(20, 30),
            range(20, 30)
        ).return
    )
    .await
    .collect::<Vec<i32>>()
    .await;

    assert_eq!(ints_with_duplicates.len(), 40);

    assert_eq!(
        ints_with_duplicates.into_iter().collect::<HashSet<i32>>(),
        (0..30).collect::<HashSet<i32>>()
    );
    println!("run complete");
}

#[cfg(feature = "tokio")]
#[tokio::main]
async fn main() {
    run().await
}

#[cfg(feature = "async-std")]
#[async_std::main]
async fn main() {
    run().await
}

#[cfg(not(any(feature = "async-std", feature = "tokio")))]
#[tokio::main(flavor = "current_thread")]
async fn main() {
    run().await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(not(feature = "async-std"))]
    #[tokio::test]
    async fn multi_consumer() {
        run().await;
    }

    #[cfg(feature = "async-std")]
    #[async_std::test]
    async fn multi_consumer() {
        run().await;
    }
}
