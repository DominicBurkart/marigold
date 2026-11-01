use marigold::m;
use marigold::marigold_impl::StreamExt;
use std::time::Duration;

fn f(a: i32) -> i32 {
    a * 10
}

fn g(a: i32) -> i32 {
    a + 1
}

fn even(a: i32) -> i32 {
    a * 2
}

fn odd(a: i32) -> i32 {
    a * 2 + 1
}

fn keep(a: i32) -> bool {
    a % 2 == 0
}

fn slow(a: i32) -> i32 {
    std::thread::sleep(Duration::from_millis(1));
    a
}

fn add(a: i32, b: i32) -> i32 {
    a + b
}

fn first(v: [i32; 2]) -> i32 {
    v[0]
}

fn compare(a: &i32, b: &i32) -> std::cmp::Ordering {
    a.cmp(b)
}

async fn bounded<S: futures::Stream<Item = i32> + Unpin>(s: S) -> Vec<i32> {
    tokio::time::timeout(Duration::from_secs(10), s.collect::<Vec<_>>())
        .await
        .expect("stream did not terminate")
}

fn sorted(mut v: Vec<i32>) -> Vec<i32> {
    v.sort();
    v
}

macro_rules! same_multiset {
    ($name:ident, named { $($n:tt)* }, inline { $($i:tt)* }) => {
        #[tokio::test(flavor = "multi_thread")]
        async fn $name() {
            let named = bounded(m!($($n)*).await).await;
            let inlined = bounded(m!($($i)*).await).await;
            assert_eq!(sorted(named), sorted(inlined));
        }
    };
}

same_multiset!(
    fan_out_two_consumers,
    named {
        x = range(0, 20)
        x.map(f).return
        x.map(g).return
    },
    inline {
        range(0, 20).map(f).return
        range(0, 20).map(g).return
    }
);

same_multiset!(
    same_consumer_twice,
    named {
        x = range(0, 20)
        x.map(f).return
        x.map(f).return
    },
    inline {
        range(0, 20).map(f).return
        range(0, 20).map(f).return
    }
);

same_multiset!(
    zero_function_consumers,
    named {
        x = range(0, 20)
        x.return
        x.return
    },
    inline {
        range(0, 20).return
        range(0, 20).return
    }
);

same_multiset!(
    chain_of_three,
    named {
        x = range(0, 20)
        y = x.map(f)
        z = y.map(g)
        z.return
    },
    inline {
        range(0, 20).map(f).map(g).return
    }
);

same_multiset!(
    diamond,
    named {
        x = range(0, 20)
        y = x.map(f)
        z = x.filter(keep)
        y.return
        z.return
    },
    inline {
        range(0, 20).map(f).return
        range(0, 20).filter(keep).return
    }
);

same_multiset!(
    fan_out_of_5000_items_to_three_consumers,
    named {
        x = range(0, 5000)
        x.map(f).return
        x.map(g).return
        x.filter(keep).return
    },
    inline {
        range(0, 5000).map(f).return
        range(0, 5000).map(g).return
        range(0, 5000).filter(keep).return
    }
);

same_multiset!(
    slow_consumer_does_not_lose_items,
    named {
        x = range(0, 200)
        x.map(slow).return
        x.map(g).return
    },
    inline {
        range(0, 200).map(slow).return
        range(0, 200).map(g).return
    }
);

same_multiset!(
    fold_consumer_and_streaming_consumer,
    named {
        x = range(0, 10)
        x.fold(0, add).return
        x.map(g).return
    },
    inline {
        range(0, 10).fold(0, add).return
        range(0, 10).map(g).return
    }
);

same_multiset!(
    unused_variable_changes_nothing,
    named {
        x = range(0, 10)
        range(0, 3).return
    },
    inline {
        range(0, 3).return
    }
);

#[tokio::test(flavor = "multi_thread")]
async fn each_consumer_sees_its_items_in_order() {
    let out = bounded(
        m!(
            x = range(0, 200)
            x.map(even).return
            x.map(odd).return
        )
        .await,
    )
    .await;
    let evens: Vec<i32> = out.iter().copied().filter(|v| v % 2 == 0).collect();
    let odds: Vec<i32> = out.iter().copied().filter(|v| v % 2 == 1).collect();
    assert_eq!(evens, (0..200).map(even).collect::<Vec<_>>());
    assert_eq!(odds, (0..200).map(odd).collect::<Vec<_>>());
}

#[tokio::test(flavor = "multi_thread")]
async fn dropping_the_output_early_terminates() {
    let s = m!(
        x = range(0, 100000)
        x.map(f).return
        x.map(g).return
    )
    .await;
    let taken = tokio::time::timeout(Duration::from_secs(10), s.take(3).collect::<Vec<_>>())
        .await
        .expect("take did not terminate");
    assert_eq!(taken.len(), 3);
}

macro_rules! hangs_when_named {
    ($name:ident, $reason:literal, named { $($n:tt)* }, inline { $($i:tt)* }) => {
        #[ignore = $reason]
        #[tokio::test(flavor = "multi_thread")]
        async fn $name() {
            let inlined = bounded(m!($($i)*).await).await;
            let named = tokio::time::timeout(Duration::from_secs(3), async {
                m!($($n)*).await.collect::<Vec<_>>().await
            })
            .await
            .expect("named form hangs");
            assert_eq!(sorted(named), sorted(inlined));
        }
    };
}

hangs_when_named!(
    collecting_permutations_consumer,
    "see #436",
    named {
        x = range(0, 5)
        x.permutations(2).map(first).return
    },
    inline {
        range(0, 5).permutations(2).map(first).return
    }
);

hangs_when_named!(
    collecting_keep_first_n_consumer,
    "see #436",
    named {
        x = range(0, 5)
        x.keep_first_n(2, compare).return
    },
    inline {
        range(0, 5).keep_first_n(2, compare).return
    }
);

hangs_when_named!(
    collecting_permutations_with_replacement_consumer,
    "see #436",
    named {
        x = range(0, 3)
        x.permutations_with_replacement(2).map(first).return
    },
    inline {
        range(0, 3).permutations_with_replacement(2).map(first).return
    }
);

hangs_when_named!(
    variable_declared_through_a_collecting_step,
    "see #436",
    named {
        x = range(0, 3)
        y = x.permutations(2)
        y.map(first).return
    },
    inline {
        range(0, 3).permutations(2).map(first).return
    }
);

hangs_when_named!(
    collecting_consumer_next_to_streaming_consumer,
    "see #436",
    named {
        x = range(0, 5)
        x.permutations(2).map(first).return
        x.return
    },
    inline {
        range(0, 5).permutations(2).map(first).return
        range(0, 5).return
    }
);
