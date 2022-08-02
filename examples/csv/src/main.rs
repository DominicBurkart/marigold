#[allow(unused_imports)]
use marigold::m;
use serde::Deserialize;
use serde::Serialize;

#[derive(Eq, PartialEq, Serialize, Deserialize, Debug)]
struct Ship {
    class: String,
    hull: String,
}

#[allow(dead_code)]
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

#[allow(dead_code)]
fn is_spherical(ship: &Ship) -> bool {
    ship.hull == "spherical"
}

/// Uses a multithread tokio runtime and tokio-console.
#[cfg(all(feature = "tokio", feature = "io"))]
#[tokio::main]
async fn main() {
    console_subscriber::init();

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
        .ok()
        .filter(is_spherical)
        .to_vec()
        .return
    )
    .await;
    println!("Best classes (deserialized): {:?}", ships);

    // read records by passing a schema struct
    let ships = m!(
        read_file("./data/compressed.csv.gz", csv, struct=Ship)
            .ok_or_panic()
            .filter(is_spherical)
            .to_vec()
            .return
    )
    .await;
    println!("Best classes (deserialized, from compressed): {:?}", ships);

    // declare the struct inside marigold
    let ships = m!(
        struct Vaisseau {
            class: string_8,
            hull: string_10,
        }

        read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
            .ok_or_panic()
            .to_vec()
            .return
    )
    .await;
    println!(
        "All classes (deserialized, from compressed, using Marigold struc definition): {:?}",
        ships
    );

    // write records to csv
    m!(read_file("./data/uncompressed.csv", csv, struct=Ship)
        .ok_or_panic()
        .filter(is_spherical)
        .write_file("./output/uncompressed.csv", csv)
    )
    .await;
}

/// Returns a single future, where all computation occurs in a single thread.
#[cfg(not(feature = "io"))]
fn main() {
    println!("File I/O is currently only supported while using tokio and io.")
}

#[cfg(feature = "io")]
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
                .filter(is_spherical)
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
                .filter(is_spherical)
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

        // declare the struct inside marigold
        assert_eq!(
            m!(
                enum Hull {
                    Spherical = "spherical",
                    Split = "split",
                }

                struct Vaisseau {
                    class: string_8,
                    hull: Hull,
                }

                read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
                    .ok_or_panic()
                    .to_vec()
                    .return
            )
            .await
            .len(),
            3
        )
    }
}
