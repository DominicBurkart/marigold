#[allow(unused_imports)]
use marigold::m;
#[allow(unused_imports)]
use marigold::marigold_impl::StreamExt;
use serde::Deserialize;
use serde::Serialize;

#[derive(Clone, PartialEq, Eq, Serialize, Deserialize, Debug)]
struct Ship {
    class: String,
    hull: String,
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

    // read records by passing a schema struct
    let ships = m!(
        read_file("./data/uncompressed.csv", csv, struct=Ship)
        .ok()
        .filter(is_spherical)
        .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    println!("Best classes (deserialized): {:?}", ships);

    // read records by passing a schema struct
    let ships = m!(
        read_file("./data/compressed.csv.gz", csv, struct=Ship)
            .ok_or_panic()
            .filter(is_spherical)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    println!("Best classes (deserialized, from compressed): {:?}", ships);

    // declare the struct inside marigold
    let ships = m!(
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
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    println!(
        "All classes (deserialized, from compressed, using Marigold struct definition): {:?}",
        ships
    );

    // write records to csv
    m!(
        read_file("./data/uncompressed.csv", csv, struct=Ship)
            .ok_or_panic()
            .filter(is_spherical)
            .write_file("./output/is_spherical.csv", csv)

        read_file("./data/uncompressed.csv", csv, struct=Ship)
            .ok_or_panic()
            .write_file("./output/uncompressed_copy.csv", csv)
    )
    .await;

    let v = m!(

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
            .return

        read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
            .ok_or_panic()
            .write_file("./output/uncompressed.csv", csv)
    )
    .await
    .collect::<Vec<_>>()
    .await;
    assert_eq!(v.len(), 3);
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
    async fn deserialize_and_filter() {
        let inp = "./data/uncompressed.csv";
        assert_eq!(
            m!(
                read_file(inp, csv, struct=Ship)
                .ok_or_panic()
                .filter(is_spherical)
                .return
            )
            .await
            .collect::<Vec<_>>()
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
                .return
            )
            .await
            .collect::<Vec<_>>()
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
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await
            .len(),
            3
        );

        // stream terminating .write_file can go after .return stream
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
                    .return

                read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
                    .ok_or_panic()
                    .write_file("./output/uncompressed.csv", csv)
            )
            .await
            .collect::<Vec<_>>()
            .await
            .len(),
            3
        );
    }

    #[tokio::test]
    async fn write_compressed() {
        // compress
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
                .write_file("./output/compressed.csv.gz", csv, compression=gz)

            read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
                .ok_or_panic()
                .write_file("./output/also_compressed.csv.gz", csv)
        )
        .await;

        // decompress
        m!(
            enum Hull {
                Spherical = "spherical",
                Split = "split",
            }

            struct Vaisseau {
                class: string_8,
                hull: Hull,
            }

            read_file("./output/compressed.csv.gz", csv, struct=Vaisseau)
                .ok_or_panic()
                .write_file("./output/decompressed.csv", csv, compression=none)

            read_file("./output/also_compressed.csv.gz", csv, struct=Vaisseau)
                .ok_or_panic()
                .write_file("./output/also_decompressed.csv", csv)
        )
        .await;

        async fn read_csv(path: &str) -> Vec<csv_async::StringRecord> {
            use marigold::marigold_impl::TokioAsyncReadCompatExt;

            csv_async::AsyncReader::from_reader(
                tokio::fs::File::open(path)
                    .await
                    .expect("Could not read CSV file")
                    .compat(),
            )
            .into_records()
            .map(|r| r.expect("could not deserialize row"))
            .collect::<Vec<_>>()
            .await
        }

        // compare round-trip from both write paths
        assert_eq!(
            read_csv("./output/decompressed.csv").await,
            read_csv("./output/also_decompressed.csv").await
        );

        // compare round-trip to source data
        assert_eq!(
            read_csv("./data/uncompressed.csv").await,
            read_csv("./output/decompressed.csv").await
        );
    }
}
