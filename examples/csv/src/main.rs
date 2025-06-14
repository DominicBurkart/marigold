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

    /// Here, we test that the `default` variant in enums operates as expected.
    #[cfg(not(feature = "io"))]
    #[tokio::test]
    async fn default_deserialize() {
        // declaration does not error or cause other variants to deserialize incorrectly
        assert_eq!(
            m!(
                enum Hull {
                    Spherical = "spherical",
                    default Other,
                }

                struct Vaisseau {
                    class: string_8,
                    hull: Hull,
                }

                fn is_spherical(v: &Vaisseau) -> bool {
                    match v.hull {
                        Hull::Spherical => true,
                        _ => false
                    }
                }

                read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
                    .ok_or_panic()
                    .filter(is_spherical)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await
            .len(),
            2
        );

        // same declaration with a default serialization value
        assert_eq!(
            m!(
                enum Hull {
                    Spherical = "spherical",
                    default Other = "other"
                }

                struct Vaisseau {
                    class: string_8,
                    hull: Hull,
                }

                fn is_spherical(v: &Vaisseau) -> bool {
                    match v.hull {
                        Hull::Spherical => true,
                        _ => false
                    }
                }

                read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
                    .ok_or_panic()
                    .filter(is_spherical)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await
            .len(),
            2
        );

        // we can retain the content that we deserialized as the default variant
        assert_eq!(
            m!(
                enum Hull {
                    Spherical = "spherical",
                    default Other(string_10),
                }

                struct Vaisseau {
                    class: string_8,
                    hull: Hull,
                }

                fn is_other_hull(v: &Vaisseau) -> bool {
                    match v.hull {
                        Hull::Spherical => false,
                        Hull::Other(_) => true
                    }
                }

                fn get_hull_value(v: Vaisseau) -> string_10 {
                    match v.hull {
                        Hull::Spherical => panic!("Should not call get_hull_value on Spherical"),
                        Hull::Other(hull_value) => hull_value
                    }
                }

                read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
                    .ok_or_panic()
                    .filter(is_other_hull)
                    .map(get_hull_value)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await,
            vec![marigold::marigold_impl::arrayvec::ArrayString::<10>::from("split").unwrap()]
        );

        // errors due to input being smaller than the default max value size don't break everything
        assert_eq!(
            m!(
                enum Hull {
                    Spherical = "spherical",
                    default Other(string_1),
                }

                struct Vaisseau {
                    class: string_8,
                    hull: Hull,
                }

                fn is_spherical(v: &Vaisseau) -> bool {
                    match v.hull {
                        Hull::Spherical => true,
                        _ => false
                    }
                }

                read_file("./data/compressed.csv.gz", csv, struct=Vaisseau)
                    .ok()
                    .filter(is_spherical)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await
            .len(),
            2
        );
    }

    #[cfg(not(feature = "io"))]
    #[tokio::test]
    async fn missing_values() {
        assert_eq!(
            m!(
                enum Hull {
                    Spherical = "spherical",
                    default Other = "other"
                }

                struct Vaisseau {
                    class: Option<string_8>,
                    hull: Hull,
                }

                fn class(v: Vaisseau) -> Option<string_8> {
                    v.class
                }

                read_file("./data/with_missing_values.csv", csv, struct=Vaisseau)
                    .ok_or_panic()
                    .map(class)
                    .return
            )
            .await
            .collect::<Vec<_>>()
            .await,
            vec![
                Some(
                    marigold::marigold_impl::arrayvec::ArrayString::<8>::from("Deadalus").unwrap()
                ),
                Some(marigold::marigold_impl::arrayvec::ArrayString::<8>::from("Oberth").unwrap()),
                None
            ]
        )
    }
}
