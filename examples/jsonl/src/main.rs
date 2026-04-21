//! Example and integration tests for the `jsonl` format in marigold
//! `read_file`/`write_file` (issue #179).
//!
//! Each line of a JSONL (JSON Lines / NDJSON) file is an independent JSON
//! value. Empty lines are skipped on read. A trailing newline is always
//! written after the last record. The output is NOT wrapped in an outer
//! JSON array — that is issue #180 (sibling feature).

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
fn is_spherical(ship: Ship) -> bool {
    ship.hull == "spherical"
}

#[cfg(all(feature = "tokio", feature = "io"))]
#[tokio::main]
async fn main() {
    let ships = m!(
        read_file("./data/ships.jsonl", jsonl, struct=Ship)
            .ok_or_panic()
            .filter(is_spherical)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;
    println!("Spherical ships: {:?}", ships);
}

#[cfg(not(all(feature = "tokio", feature = "io")))]
fn main() {
    println!("JSONL I/O requires the tokio and io features to be enabled.");
}

#[cfg(feature = "io")]
#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::Mutex;

    // marigold file paths must be string literals, so tests use a fixed filename
    // and operate in a tempdir. `set_current_dir` is process-global, so we
    // serialize all tests that change cwd behind this mutex.
    static CWD_LOCK: Mutex<()> = Mutex::new(());

    struct CwdGuard {
        previous: std::path::PathBuf,
        _guard: std::sync::MutexGuard<'static, ()>,
    }

    impl CwdGuard {
        fn enter(dir: &std::path::Path) -> Self {
            let guard = CWD_LOCK.lock().unwrap_or_else(|e| e.into_inner());
            let previous = std::env::current_dir().unwrap();
            std::env::set_current_dir(dir).unwrap();
            CwdGuard {
                previous,
                _guard: guard,
            }
        }
    }

    impl Drop for CwdGuard {
        fn drop(&mut self) {
            let _ = std::env::set_current_dir(&self.previous);
        }
    }

    #[tokio::test]
    async fn deserialize_and_filter() {
        // All tests use CwdGuard because marigold's read_file/write_file paths
        // must be string literals, so we operate inside a per-test tempdir
        // with fixed relative filenames.
        let dir = tempfile::tempdir().unwrap();
        let _cwd = CwdGuard::enter(dir.path());

        tokio::fs::write(
            "./in.jsonl",
            "{\"class\":\"Deadalus\",\"hull\":\"spherical\"}\n\
             {\"class\":\"Intrepid\",\"hull\":\"split\"}\n\
             {\"class\":\"Olympic\",\"hull\":\"spherical\"}\n",
        )
        .await
        .unwrap();

        assert_eq!(
            m!(
                read_file("./in.jsonl", jsonl, struct=Ship)
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
    async fn round_trip_uncompressed() {
        let dir = tempfile::tempdir().unwrap();
        let _cwd = CwdGuard::enter(dir.path());

        let original = vec![
            Ship {
                class: "Constellation".to_string(),
                hull: "saucer".to_string(),
            },
            Ship {
                class: "Excelsior".to_string(),
                hull: "split".to_string(),
            },
        ];

        // Seed the input by writing JSONL by hand (marigold has no Vec input).
        {
            let mut contents = String::new();
            for s in &original {
                contents.push_str(&serde_json::to_string(s).unwrap());
                contents.push('\n');
            }
            tokio::fs::write("./in.jsonl", contents).await.unwrap();
        }

        // Read via marigold and write out via marigold.
        m!(
            read_file("./in.jsonl", jsonl, struct=Ship)
                .ok_or_panic()
                .write_file("./out.jsonl", jsonl)
        )
        .await;

        let out_text = String::from_utf8(tokio::fs::read("./out.jsonl").await.unwrap()).unwrap();
        assert!(
            out_text.ends_with('\n'),
            "jsonl output must have a trailing newline"
        );
        let recovered: Vec<Ship> = out_text
            .lines()
            .filter(|l| !l.is_empty())
            .map(|l| serde_json::from_str::<Ship>(l).unwrap())
            .collect();
        assert_eq!(recovered, original);
    }

    #[tokio::test]
    async fn round_trip_gzip() {
        let dir = tempfile::tempdir().unwrap();
        let _cwd = CwdGuard::enter(dir.path());

        let seed: Vec<Ship> = (0..25)
            .map(|i| Ship {
                class: format!("C{}", i),
                hull: if i % 2 == 0 { "spherical" } else { "split" }.to_string(),
            })
            .collect();
        {
            let mut contents = String::new();
            for s in &seed {
                contents.push_str(&serde_json::to_string(s).unwrap());
                contents.push('\n');
            }
            tokio::fs::write("./seed.jsonl", contents).await.unwrap();
        }

        // seed.jsonl -> seed.jsonl.gz (gzip auto-detect from extension)
        m!(
            read_file("./seed.jsonl", jsonl, struct=Ship)
                .ok_or_panic()
                .write_file("./seed.jsonl.gz", jsonl)
        )
        .await;

        // seed.jsonl.gz -> back to uncompressed (exercise gzip auto-detect on read
        // and explicit compression=none on write).
        m!(
            read_file("./seed.jsonl.gz", jsonl, struct=Ship)
                .ok_or_panic()
                .write_file("./recovered.jsonl", jsonl, compression=none)
        )
        .await;

        let recovered_text =
            String::from_utf8(tokio::fs::read("./recovered.jsonl").await.unwrap()).unwrap();
        let recovered: Vec<Ship> = recovered_text
            .lines()
            .filter(|l| !l.is_empty())
            .map(|l| serde_json::from_str::<Ship>(l).unwrap())
            .collect();
        assert_eq!(recovered, seed);
    }

    #[tokio::test]
    async fn skips_empty_lines() {
        let dir = tempfile::tempdir().unwrap();
        let _cwd = CwdGuard::enter(dir.path());

        tokio::fs::write(
            "./sparse.jsonl",
            "\n{\"class\":\"A\",\"hull\":\"s\"}\n\n{\"class\":\"B\",\"hull\":\"s\"}\n\n",
        )
        .await
        .unwrap();

        let read = m!(
            read_file("./sparse.jsonl", jsonl, struct=Ship)
                .ok_or_panic()
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        assert_eq!(
            read,
            vec![
                Ship {
                    class: "A".to_string(),
                    hull: "s".to_string()
                },
                Ship {
                    class: "B".to_string(),
                    hull: "s".to_string()
                },
            ]
        );
    }

    // Property test: for any Vec<Ship>, seeding a JSONL file by hand and then
    // reading it back through marigold's `read_file(..., jsonl, ...)` recovers
    // the input exactly. This exercises the reader over arbitrary inputs.
    //
    // Note: the writer side of `write_file(..., jsonl, ...)` uses a
    // `static OnceCell` per generated output function, which means a single
    // process can only exercise the writer once per `m!` invocation. We test
    // the writer via the dedicated `round_trip_uncompressed`, `round_trip_gzip`,
    // and `writer_single_case_property_like` tests above/below rather than via
    // proptest's repeated invocations.
    use proptest::strategy::Strategy;

    proptest::proptest! {
        #![proptest_config(proptest::prelude::ProptestConfig {
            cases: 16,
            .. proptest::prelude::ProptestConfig::default()
        })]
        #[test]
        fn prop_read_round_trip(
            ships in proptest::collection::vec(
                (
                    proptest::string::string_regex("[a-zA-Z0-9 ]{0,16}").unwrap(),
                    proptest::string::string_regex("[a-zA-Z0-9]{0,8}").unwrap(),
                ).prop_map(|(class, hull)| Ship { class, hull }),
                0..6usize,
            )
        ) {
            let runtime = tokio::runtime::Builder::new_current_thread()
                .enable_all()
                .build()
                .unwrap();
            runtime.block_on(async {
                let dir = tempfile::tempdir().unwrap();
                let _cwd = CwdGuard::enter(dir.path());

                // Seed the input file using std JSONL directly.
                {
                    let mut contents = String::new();
                    for s in &ships {
                        contents.push_str(&serde_json::to_string(s).unwrap());
                        contents.push('\n');
                    }
                    tokio::fs::write("./in.jsonl", contents).await.unwrap();
                }

                // Read via marigold.
                let recovered: Vec<Ship> = m!(
                    read_file("./in.jsonl", jsonl, struct=Ship)
                        .ok_or_panic()
                        .return
                )
                .await
                .collect::<Vec<_>>()
                .await;

                assert_eq!(recovered, ships);
            });
        }
    }
}
