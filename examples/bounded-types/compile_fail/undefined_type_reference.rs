use marigold::m;
use marigold::marigold_impl::StreamExt;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let _ = m!(
        struct Sensor {
            reading: int[0, NonExistent.len()]
        }
        range(0, 1).return
    )
    .await
    .collect::<Vec<()>>()
    .await;
}
