use marigold::m;
use marigold::marigold_impl::StreamExt;

async fn fold_u8() {
    let sum_of_ints = m!(
        fn add(acc: u8, item: u8) -> u8 %%%MARIGOLD_FUNCTION_START%%%
            acc + item
        %%%MARIGOLD_FUNCTION_END%%%

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

async fn fold_struct() {
    // TODO: Fix post-fold map transformation
    println!("fold_struct skipped for now")
}

async fn run_all() {
    fold_u8().await;
    fold_struct().await;
    println!("all ran");
}

#[cfg(feature = "tokio")]
#[tokio::main]
async fn main() {
    run_all().await;
}

#[cfg(feature = "async-std")]
#[async_std::main]
async fn main() {
    run_all().await;
}

#[cfg(not(any(feature = "async-std", feature = "tokio")))]
#[tokio::main(flavor = "current_thread")]
async fn main() {
    run_all().await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(not(feature = "async-std"))]
    #[tokio::test]
    async fn stateful() {
        run_all().await;
    }

    #[cfg(feature = "async-std")]
    #[async_std::test]
    async fn stateful() {
        run_all().await;
    }
}
