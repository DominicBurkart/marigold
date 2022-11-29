use marigold::m;
use marigold::marigold_impl::StreamExt;

async fn run() {
    assert_eq!(
        m!(
          fn sum(acc: u32, v: &u32) -> u32 {
              acc + v
          }

          range(0, 5)
            .reduce(sum)
            .return
        )
        .await
        .collect::<Vec<u32>>()
        .await,
        vec![0 + 1 + 2 + 3 + 4]
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
