program!("tsp_brute_force" => i32);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;
    use crate::tsp;

    #[test]
    fn matches_shared_fixture_optimum() {
        assert_eq!(block_on(run()), vec![tsp::optimum() as i32]);
    }
}
