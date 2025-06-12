use marigold::m;
use marigold::marigold_impl::StreamExt;
use std::collections::HashSet;

fn is_odd(i: &i32) -> bool {
    i % 2 == 1
}

fn is_even(i: &i32) -> bool {
    i % 2 == 0
}

async fn run() {
    let some_small_even_numbers = m!(
        fn doubled_plus_ten(i: i32) -> i32 {
            (i * 2) + 10
        }

        digits = range(0, 10)

        digits
            .filter(is_even)
            .return
    )
    .await;

    let small_even_number_set = some_small_even_numbers.collect::<HashSet<i32>>().await;

    assert_eq!(
        small_even_number_set,
        [0, 2, 4, 6, 8]
            .into_iter()
            .collect::<HashSet<i32>>()
    );

    println!("result: {:?}", small_even_number_set);
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
