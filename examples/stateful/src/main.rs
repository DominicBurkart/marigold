use marigold::m;
use marigold::marigold_impl::StreamExt;

async fn fold_u8() {
    let sum_of_ints = m!(
        fn add(acc: u8, item: u8) -> u8 {
            acc + item
        }

        range(0, 5)
            .fold(0, add)
            .return
    )
    .await
    .collect::<Vec<u8>>()
    .await;

    assert_eq!(sum_of_ints, vec![10]);

    println!("fold_u8 complete");
}

#[cfg(feature = "tokio")]
#[tokio::main]
async fn main() {
    fold_u8().await
}

#[cfg(feature = "async-std")]
#[async_std::main]
async fn main() {
    fold_u8().await
}

#[cfg(not(any(feature = "async-std", feature = "tokio")))]
#[tokio::main(flavor = "current_thread")]
async fn main() {
    fold_u8().await
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(not(feature = "async-std"))]
    #[tokio::test]
    async fn stateful() {
        fold_u8().await;
    }

    #[cfg(feature = "async-std")]
    #[async_std::test]
    async fn stateful() {
        fold_u8().await;
    }
}
