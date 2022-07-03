use marigold::m;
use serde::Serialize;

#[derive(Serialize)]
struct Ship {
    class: String,
    hull: String,
}

async fn spherical_hull_class_names(
    rec: csv_async::Result<csv_async::StringRecord>,
) -> Option<String> {
    if let Ok(r) = rec {
        if r.get(1).unwrap() == "spherical" {
            return Some(r.get(0).unwrap().to_owned());
        }
    } else {
        eprintln!("Could not read line of CSV: {:?}", rec);
    }
    None
}

#[cfg(feature = "tokio")]
async fn only_spherical_hulls(rec: csv_async::Result<csv_async::StringRecord>) -> Option<Ship> {
    if let Ok(r) = rec {
        let hull = r.get(1).unwrap();
        if hull == "spherical" {
            return Some(Ship {
                class: r.get(0).unwrap().to_string(),
                hull: hull.to_string(),
            });
        }
    } else {
        eprintln!("Could not read line of CSV: {:?}", rec);
    }
    None
}

/// Uses a multithread tokio runtime and tokio-console.
#[cfg(feature = "tokio")]
#[tokio::main]
async fn main() {
    console_subscriber::init();

    println!(
        "Best classes: {:?}",
        m!(
            read_file("./data/uncompressed.csv", csv)
            .filter_map(spherical_hull_class_names)
            .to_vec()
            .return
        )
        .await
    );

    m!(read_file("./data/uncompressed.csv", csv)
        .filter_map(only_spherical_hulls)
        .write_file("./output/uncompressed.csv", csv))
    .await;
}

/// Uses a multithread async-std runtime.
#[cfg(feature = "async-std")]
#[async_std::main]
async fn main() {
    println!(
        "Best classes: {:?}",
        m!(
            read_file("./data/uncompressed.csv", csv)
            .filter_map(spherical_hull_class_names)
            .to_vec()
            .return
        )
        .await
    );
}

/// Returns a single future, where all computation occurs in a single thread.
#[cfg(not(any(feature = "async-std", feature = "tokio", target_family = "wasm")))]
fn main() {
    use futures::executor::LocalPool;

    let mut pool = LocalPool::new();

    println!(
        "Best classes: {:?}",
        pool.run_until(m!(
            read_file("./data/uncompressed.csv", csv)
            .filter_map(spherical_hull_class_names)
            .to_vec()
            .return
        ))
    );
}

/// On wasm, file system support does not work out-of-the-box with async-std.
#[cfg(target_family = "wasm")]
fn main() {}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn read_csv_and_filter_map() {
        assert_eq!(
            m!(
                read_file("./data/uncompressed.csv", csv)
                .filter_map(spherical_hull_class_names)
                .to_vec()
                .return
            )
            .await,
            vec!["Deadalus", "Olympic"]
        );
    }
}
