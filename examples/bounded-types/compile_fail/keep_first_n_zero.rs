use marigold::m;
use marigold::marigold_impl::StreamExt;

#[tokio::main(flavor = "current_thread")]
async fn main() {
    let _ = m!(
        range(0, 10).keep_first_n(0, compare).return
    )
    .await
    .collect::<Vec<()>>()
    .await;
}

fn compare(a: &i32, b: &i32) -> std::cmp::Ordering {
    a.cmp(b)
}
