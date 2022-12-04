use futures::stream::Stream;
use futures::stream::StreamExt;
use futures::SinkExt;
use genawaiter::sync::Co;
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::Hash;
use std::hash::Hasher;
use std::pin::Pin;

const BUFFER_SIZE: usize = 1;

pub trait PartitionByHash<SInput, T, Fun> {
    fn partition_by_hash(
        self,
        partitioning_function: Fun,
    ) -> genawaiter::sync::Gen<
        futures::channel::mpsc::Receiver<T>,
        (),
        Pin<Box<(dyn futures::Future<Output = ()> + std::marker::Send + 'static)>>,
    >;
}

/// Partition a stream by applying a function that returns a hashable value,
/// then routing each item from the original stream to the corresponding hash.
///
/// Note that this abstraction is prone to deadlocks if the client attempts to
/// collect all of the partitions without exhausting the contents of each partition.
///
/// The number of partitions can increase up to u64 (the maximum number of unique
/// hash values) depending on how many hash-unique results are returned from
/// `partitioning_function` and senders are allocated dynamically. This can cause
/// increased memory usage over the duration of the program.
///
/// The stream for a given hash is created and returned as soon as an item with
/// that hash is encountered. Backpressure defined by the slowest consumer.
impl<SInput, T, Fun, Hashable> PartitionByHash<SInput, T, Fun> for SInput
where
    SInput: Stream<Item = T> + Send + Sized + std::marker::Unpin + 'static,
    T: Clone + Send + 'static,
    Fun: Fn(&T) -> Hashable + Send + 'static,
    Hashable: Hash + Send,
{
    fn partition_by_hash(
        mut self,
        partitioning_function: Fun,
    ) -> genawaiter::sync::Gen<
        futures::channel::mpsc::Receiver<T>,
        (),
        Pin<Box<(dyn futures::Future<Output = ()> + std::marker::Send + 'static)>>,
    > {
        let mut senders = HashMap::new();

        genawaiter::sync::Gen::new_boxed(|co: Co<futures::channel::mpsc::Receiver<T>>| async move {
            while let Some(v) = self.next().await {
                // hash
                let mut hasher = DefaultHasher::new();
                partitioning_function(&v).hash(&mut hasher);
                let hash = hasher.finish();

                // create and yield new partitioned stream if the hash is new
                if let std::collections::hash_map::Entry::Vacant(entry) = senders.entry(hash) {
                    let (sender, receiver) = futures::channel::mpsc::channel(BUFFER_SIZE);
                    entry.insert(sender);
                    co.yield_(receiver).await;
                }

                // send value to relevant partition
                senders
                    .get_mut(&hash)
                    .expect("logic error in while partitioning: sender for hash not found")
                    .feed(v)
                    .await
                    .expect("could not write to partition");
            }

            // close senders
            for mut sender in senders.into_values() {
                sender.disconnect();
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::PartitionByHash;
    use futures::stream::FuturesOrdered;
    use futures::stream::StreamExt;
    use std::collections::HashSet;

    #[tokio::test]
    async fn partition_by_modulo() {
        let mut partition_futs = Vec::new();
        let mut partitioned_stream = futures::stream::iter(0..=255).partition_by_hash(|&i| i % 10);

        while let Some(partition) = partitioned_stream.next().await {
            partition_futs.push(tokio::spawn(
                async move { partition.collect::<Vec<_>>().await },
            ));
        }

        let result = partition_futs
            .into_iter()
            .collect::<FuturesOrdered<_>>()
            .map(|f| f.expect("could not join"))
            .collect::<Vec<Vec<u8>>>()
            .await;

        // split into ten partitions
        assert_eq!(result.len(), 10);

        // identical contents as input
        assert_eq!(
            result.into_iter().flatten().collect::<HashSet<u8>>(),
            (0..=255).collect::<HashSet<u8>>()
        );
    }

    #[tokio::test]
    async fn partition_identity() {
        assert_eq!(
            futures::stream::iter(0..5)
                .partition_by_hash(|&i| i)
                .map(|s| async { s.collect::<Vec<_>>().await })
                .collect::<FuturesOrdered::<_>>()
                .await
                .collect::<Vec<Vec<u8>>>()
                .await,
            vec![vec![0], vec![1], vec![2], vec![3], vec![4]]
        );
    }

    #[tokio::test]
    async fn partition_custom_struct_by_hash() {
        #[derive(PartialEq, Eq, Clone, Debug)]
        struct Meow {
            volume: u64,
            source_cat_id: u8,
        }

        assert_eq!(
            futures::stream::iter(vec![
                Meow {
                    volume: 10,
                    source_cat_id: 1
                },
                Meow {
                    volume: 23210,
                    source_cat_id: 2
                },
                Meow {
                    volume: 0,
                    source_cat_id: 3
                },
            ])
            .partition_by_hash(|m| m.source_cat_id)
            .map(|s| async { s.collect::<Vec<_>>().await })
            .collect::<FuturesOrdered::<_>>()
            .await
            .collect::<Vec<Vec<_>>>()
            .await,
            vec![
                vec![Meow {
                    volume: 10,
                    source_cat_id: 1
                }],
                vec![Meow {
                    volume: 23210,
                    source_cat_id: 2
                }],
                vec![Meow {
                    volume: 0,
                    source_cat_id: 3
                }],
            ]
        );
    }

    #[tokio::test]
    async fn partition_does_not_deadlock_with_additional_inputs() {
        assert_eq!(
            futures::stream::iter(0..100_000)
                .partition_by_hash(|&i| i)
                .map(|s| async { s.collect::<Vec<_>>().await })
                .collect::<FuturesOrdered::<_>>()
                .await
                .collect::<Vec<Vec<u32>>>()
                .await
                .len(),
            100_000
        );
    }
}
