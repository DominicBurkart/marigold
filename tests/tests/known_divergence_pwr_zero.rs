use marigold::m;
use marigold::marigold_impl::StreamExt;
use marigold_grammar::complexity::Cardinality;
use marigold_grammar::sizes::{InputSizes, ResolutionKind};
use marigold_grammar::{marigold_analyze, marigold_analyze_with_sizes};
use num_bigint::BigUint;

const CONSTANT: &str = "range(0, 3).permutations_with_replacement(0).return";
const UNBOUND: &str = "struct D { x: i32 }\nread_file(\"d.csv\", csv, struct=D).ok_or_panic().permutations_with_replacement(0).return";

#[tokio::test]
async fn runtime_yields_no_items_for_zero_length_replacement_permutations() {
    for n in [0usize, 1, 3] {
        let items = match n {
            0 => m!(range(0, 0).permutations_with_replacement(0).return)
                .await
                .collect::<Vec<_>>()
                .await
                .len(),
            1 => m!(range(0, 1).permutations_with_replacement(0).return)
                .await
                .collect::<Vec<_>>()
                .await
                .len(),
            _ => m!(range(0, 3).permutations_with_replacement(0).return)
                .await
                .collect::<Vec<_>>()
                .await
                .len(),
        };
        assert_eq!(items, 0, "runtime item count for n={n}");
    }
}

#[test]
fn analyzer_reports_one_item_where_the_runtime_yields_none() {
    let program = marigold_analyze(CONSTANT).unwrap();
    assert_eq!(
        program.streams[0].cardinality,
        Cardinality::Exact(BigUint::from(1u32)),
        "known divergence from the runtime count of 0; see #457; flip this assertion together with the analyzer fix"
    );

    let analysis =
        marigold_analyze_with_sizes(UNBOUND, &InputSizes::new().bind(0, "n").unwrap()).unwrap();
    let last = analysis.streams[0].stages.last().unwrap();
    assert_eq!(last.resolution.kind(), ResolutionKind::Symbolic);
    assert_eq!(last.resolution.lo().to_string(), "$n^0");
    assert_eq!(last.resolution.hi().unwrap().to_string(), "$n^0");
}
