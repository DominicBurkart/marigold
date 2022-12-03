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

async fn fold_struct() {
    assert_eq!(
        m!(
            struct Meow {
                volume: u32
            }

            fn make_accumulator() -> Meow {
                Meow {volume: 10}
            }

            fn accumulate(meow: Meow, item: u32) -> Meow {
                Meow {
                    volume: meow.volume + item
                }
            }

            fn unwrap_meow(meow: Meow) -> u32 {
                meow.volume
            }

            range(0, 5)
                .fold(make_accumulator, accumulate)
                .map(unwrap_meow)
                .return
        )
        .await
        .collect::<Vec<u32>>()
        .await,
        vec![20]
    );

    println!("fold_struct complete")
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
