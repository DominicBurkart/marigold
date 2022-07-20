use marigold::m;
use serde::Deserialize;
use serde::Serialize;

#[derive(Eq, PartialEq, Serialize, Deserialize, Debug)]
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

async fn only_spherical_hulls(ship: Ship) -> Option<Ship> {
    if ship.hull == "spherical" {
        return Some(ship);
    }
    None
}

/// Uses a multithread tokio runtime and tokio-console.
#[cfg(feature = "tokio")]
#[tokio::main]
async fn main() {
    console_subscriber::init();

    // // read records without passing a schema struct
    // println!(
    //     "Best classes: {:?}",
    //     m!(
    //         read_file("./data/uncompressed.csv", csv)
    //         .filter_map(spherical_hull_class_names)
    //         .to_vec()
    //         .return
    //     )
    //     .await
    // );
    //
    // // read records by passing a schema struct
    // let ships = m!(
    //     read_file("./data/uncompressed.csv", csv, struct=Ship)
    //     .ok_or_panic()
    //     .filter_map(only_spherical_hulls)
    //     .to_vec()
    //     .return
    // )
    // .await;
    // println!("Best classes (deserialized): {:?}", ships);

    // read records by passing a schema struct
    let ships = m!(
        read_file("./data/compressed.csv.gz", csv, struct=Ship)
            .ok_or_panic()
            .filter_map(only_spherical_hulls)
            .to_vec()
            .return
    )
    .await;
    println!("Best classes (deserialized, from compressed): {:?}", ships);

    // // write records to csv
    // m!(read_file("./data/uncompressed.csv", csv, struct=Ship)
    //     .ok_or_panic()
    //     .filter_map(only_spherical_hulls)
    //     .write_file("./output/uncompressed.csv", csv)
    // )
    // .await;
}

/// Uses a multithread async-std runtime.
#[cfg(feature = "async-std")]
#[async_std::main]
async fn main() {
    // read records without passing a schema struct
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

    // read records by passing a schema struct
    let ships = m!(
        read_file("./data/uncompressed.csv", csv, struct=Ship)
        .ok_or_panic()
        .filter_map(only_spherical_hulls)
        .to_vec()
        .return
    )
    .await;
    println!("Best classes (deserialized): {:?}", ships);

    // write records to csv
    m!(
        read_file("./data/uncompressed.csv", csv, struct=Ship)
            .ok_or_panic()
            .filter_map(only_spherical_hulls)
            .write_file("./output/uncompressed.csv", csv)
    )
    .await;
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

    // read records by passing a schema struct
    let ships = pool.run_until(m!(
        read_file("./data/uncompressed.csv", csv, struct=Ship)
            .ok_or_panic()
            .filter_map(only_spherical_hulls)
            .to_vec()
            .return
    ));
    println!("Best classes (deserialized): {:?}", ships);
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

    #[tokio::test]
    async fn deserialize_and_filter_map() {
        assert_eq!(
            m!(
                read_file("./data/uncompressed.csv", csv, struct=Ship)
                .ok_or_panic()
                .filter_map(only_spherical_hulls)
                .to_vec()
                .return
            )
            .await,
            vec![
                Ship {
                    class: "Deadalus".to_string(),
                    hull: "spherical".to_string()
                },
                Ship {
                    class: "Olympic".to_string(),
                    hull: "spherical".to_string()
                }
            ]
        );
    }

    #[tokio::test]
    async fn read_compressed() {
        assert_eq!(
            m!(
                read_file("./data/compressed.csv.gz", csv, struct=Ship)
                .ok_or_panic()
                .filter_map(only_spherical_hulls)
                .to_vec()
                .return
            )
            .await,
            vec![
                Ship {
                    class: "Deadalus".to_string(),
                    hull: "spherical".to_string()
                },
                Ship {
                    class: "Olympic".to_string(),
                    hull: "spherical".to_string()
                }
            ]
        );
    }
}
