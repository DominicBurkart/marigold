program!("quicksort" => [i64; 16]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const UNSORTED: [i64; 16] = [29, 4, 17, 4, 88, 23, 17, 1, 56, 42, 4, 99, 23, 0, 61, 17];

    #[test]
    fn matches_std_sort() {
        let mut expected = UNSORTED;
        expected.sort();
        assert_eq!(block_on(run()), vec![expected]);
    }

    #[test]
    fn output_is_permutation_of_input_with_duplicates_kept() {
        let out = block_on(run());
        let mut a = out[0];
        let mut b = UNSORTED;
        a.sort();
        b.sort();
        assert_eq!(a, b);
        assert!(out[0].windows(2).all(|w| w[0] <= w[1]));
    }
}
