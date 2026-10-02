//! Integration tests verifying marigold's deadlock-freedom guarantee.
//!
//! Marigold's concurrency model (described in "Preventing Deadlocks in Marigold",
//! <https://dominic.computer/blog/2022/preventing_deadlocks_in_marigold>) trades
//! expressiveness for guaranteed deadlock freedom: every stream in a compiled
//! program — including the runners that drive fan-out from named stream
//! variables — is launched concurrently inside a single `select_all`, and
//! backpressure propagates through bounded channels.
//!
//! The dangerous shape is fan-out: `digits = range(...)` consumed by several
//! downstream streams. The fan-out channels buffer a single item, so with a
//! naive implementation (sequential sends to consumers, or streams driven one
//! after another) every test in this file would hang as soon as the item count
//! exceeds the buffer capacity. Each test therefore pushes hundreds of items
//! through the fan-out and is wrapped in a timeout that converts a would-be
//! deadlock into a test failure instead of a hung test runner.

use marigold::m;
use marigold::marigold_impl::StreamExt;
use std::time::Duration;

const DEADLOCK_TIMEOUT: Duration = Duration::from_secs(60);

async fn assert_completes<F: std::future::Future>(f: F) -> F::Output {
    tokio::time::timeout(DEADLOCK_TIMEOUT, f)
        .await
        .expect("marigold program did not terminate: deadlock-freedom guarantee violated")
}

fn is_even(i: i32) -> bool {
    i % 2 == 0
}

fn is_odd(i: i32) -> bool {
    i % 2 == 1
}

fn always_true(_i: i32) -> bool {
    true
}

fn always_false(_i: i32) -> bool {
    false
}

/// Two consumers of one named stream, with far more items than the fan-out
/// channels can buffer. Runs on tokio's current-thread runtime (the default for
/// `#[tokio::test]`), where nothing but the concurrent launch of all streams
/// prevents a deadlock.
#[tokio::test]
async fn fan_out_with_more_items_than_channel_buffers() {
    let mut result = assert_completes(async {
        m!(
            digits = range(0, 1000)

            digits
                .filter(is_even)
                .return

            digits
                .filter(is_odd)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    assert_eq!(result, (0..1000).collect::<Vec<_>>());
}

/// The same fan-out must also be deadlock-free on a multi-threaded runtime.
#[tokio::test(flavor = "multi_thread", worker_threads = 2)]
async fn fan_out_with_more_items_than_channel_buffers_multi_thread() {
    let mut result = assert_completes(async {
        m!(
            digits = range(0, 1000)

            digits
                .filter(is_even)
                .return

            digits
                .filter(is_odd)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    assert_eq!(result, (0..1000).collect::<Vec<_>>());
}

/// Three consumers of one named stream. Every consumer's channel must keep
/// draining for the producer to make progress.
#[tokio::test]
async fn fan_out_three_consumers() {
    fn shift(i: i32) -> i32 {
        i + 1000
    }

    let mut result = assert_completes(async {
        m!(
            digits = range(0, 300)

            digits
                .filter(is_even)
                .return

            digits
                .filter(is_odd)
                .return

            digits
                .map(shift)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    let expected = (0..300).chain(1000..1300).collect::<Vec<_>>();
    assert_eq!(result, expected);
}

/// A named stream consumed both directly (no stream functions) and through a
/// transformation.
#[tokio::test]
async fn bare_named_stream_consumer_alongside_transformed_consumer() {
    fn negate(i: i32) -> i32 {
        -i
    }

    let mut result = assert_completes(async {
        m!(
            digits = range(1, 251)

            digits.return

            digits
                .map(negate)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    let expected = (-250..=-1).chain(1..=250).collect::<Vec<_>>();
    assert_eq!(result, expected);
}

/// Diamond topology: a stream variable derived from another stream variable
/// (`odd_digits = digits.filter(is_odd)`), with consumers on both. This is the
/// multi-consumer example from the repository, scaled far past the channel
/// buffer capacity.
#[tokio::test]
async fn diamond_fan_out_through_derived_stream_variable() {
    let mut result = assert_completes(async {
        m!(
            fn doubled_plus_ten(i: i32) -> i32 {
                (i * 2) + 10
            }

            digits = range(0, 500)

            digits
                .filter(is_even)
                .return

            odd_digits = digits
                .filter(is_odd)

            odd_digits
                .map(doubled_plus_ten)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    let mut expected = (0..500)
        .filter(|i| i % 2 == 0)
        .chain((0..500).filter(|i| i % 2 == 1).map(|i| (i * 2) + 10))
        .collect::<Vec<_>>();
    expected.sort_unstable();
    assert_eq!(result, expected);
}

/// A chain of derived stream variables, each level fanning out to its own
/// consumer. Every intermediate fan-out runner must be driven concurrently.
#[tokio::test]
async fn chained_derived_stream_variables_each_with_consumers() {
    fn double(i: i32) -> i32 {
        i * 2
    }
    fn increment(i: i32) -> i32 {
        i + 1
    }

    let mut result = assert_completes(async {
        m!(
            digits = range(0, 200)

            digits.return

            doubled = digits
                .map(double)

            doubled.return

            incremented = doubled
                .map(increment)

            incremented.return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    let mut expected = (0..200)
        .chain((0..200).map(|i| i * 2))
        .chain((0..200).map(|i| i * 2 + 1))
        .collect::<Vec<_>>();
    expected.sort_unstable();
    assert_eq!(result, expected);
}

/// A consumer whose filter rejects every item never yields anything, but it must
/// still drain its channel; otherwise the producer would stall permanently and
/// starve the surviving consumer.
#[tokio::test]
async fn consumer_that_yields_nothing_still_drains_its_channel() {
    let mut result = assert_completes(async {
        m!(
            digits = range(0, 1000)

            digits
                .filter(always_false)
                .return

            digits
                .filter(always_true)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    assert_eq!(result, (0..1000).collect::<Vec<_>>());
}

/// A folding consumer emits nothing until the source is exhausted. While it
/// accumulates, its sibling consumer and the fan-out runner must keep making
/// progress, and vice versa.
#[tokio::test]
async fn folding_consumer_alongside_passthrough_consumer() {
    fn add(acc: i32, v: i32) -> i32 {
        acc + v
    }

    let mut result = assert_completes(async {
        m!(
            digits = range(0, 1000)

            digits
                .fold(0, add)
                .return

            digits
                .filter(always_true)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    let sum: i32 = (0..1000).sum();
    let mut expected = (0..1000).chain(std::iter::once(sum)).collect::<Vec<_>>();
    expected.sort_unstable();
    assert_eq!(result, expected);
}

/// Eager combinators exhaust their input before yielding (`keep_first_n`,
/// `combinations`, `permutations`). Their generated code used to await inline
/// while the consumer expressions were being evaluated — *before* the program's
/// streams were merged into the `select_all` — so the fan-out runner was never
/// polled and the program hung. The codegen now defers the draining combinator
/// chain into a lazily flattened stream driven by the select_all.
#[tokio::test]
async fn eager_keep_first_n_on_named_stream_terminates() {
    let cmp = |a: &i32, b: &i32| a.cmp(b);
    let mut result = assert_completes(async {
        m!(
            digits = range(0, 500)

            digits
                .keep_first_n(1, cmp)
                .return

            digits
                .filter(always_true)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    result.sort_unstable();
    let mut expected = (0..500).chain(std::iter::once(499)).collect::<Vec<_>>();
    expected.sort_unstable();
    assert_eq!(result, expected);
}

/// See `eager_keep_first_n_on_named_stream_terminates`.
#[tokio::test]
async fn eager_combinations_on_named_stream_terminates() {
    fn first(pair: [i32; 2]) -> i32 {
        pair[0]
    }

    let result = assert_completes(async {
        m!(
            digits = range(0, 10)

            digits
                .combinations(2)
                .map(first)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    // C(10, 2) = 45 combinations.
    assert_eq!(result.len(), 45);
}

/// See `eager_keep_first_n_on_named_stream_terminates`. Permutations shares the
/// eager `.await` codegen shape with combinations.
#[tokio::test]
async fn eager_permutations_on_named_stream_terminates() {
    fn first(pair: [i32; 2]) -> i32 {
        pair[0]
    }

    let mut result = assert_completes(async {
        m!(
            digits = range(0, 6)

            digits
                .permutations(2)
                .map(first)
                .return

            digits
                .filter(always_true)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    // P(6, 2) = 30 permutations (each contributing its first element) plus the
    // 6 pass-through items.
    result.sort_unstable();
    let mut expected = (0..6)
        .flat_map(|a| (0..6).filter(move |b| *b != a).map(move |_| a))
        .chain(0..6)
        .collect::<Vec<_>>();
    expected.sort_unstable();
    assert_eq!(result, expected);
}

/// An eager combinator inside a *derived stream variable* (`pairs = digits
/// .combinations(2)`) used to deadlock the same way: the variable declaration
/// awaited the combinator before any runner was polled. The deferred codegen
/// applies to derived stream variables too.
#[tokio::test]
async fn eager_combinator_in_derived_stream_variable_terminates() {
    fn first(pair: [i32; 2]) -> i32 {
        pair[0]
    }
    fn second(pair: [i32; 2]) -> i32 {
        pair[1]
    }

    let result = assert_completes(async {
        m!(
            digits = range(0, 10)

            digits
                .filter(always_true)
                .return

            pairs = digits
                .combinations(2)

            pairs
                .map(first)
                .return

            pairs
                .map(second)
                .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    })
    .await;
    // 10 pass-through items + C(10, 2) = 45 pairs consumed by two consumers.
    assert_eq!(result.len(), 10 + 45 + 45);
}
