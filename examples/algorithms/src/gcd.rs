program!("gcd" => u64);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    fn euclid(a: u64, b: u64) -> u64 {
        if b == 0 {
            a
        } else {
            euclid(b, a % b)
        }
    }

    #[test]
    fn gcd_matches_reference_euclid() {
        assert_eq!(block_on(run()), vec![euclid(1071, 462)]);
        assert_eq!(block_on(run()), vec![21]);
    }
}
