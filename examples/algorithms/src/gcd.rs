program!("gcd" => [u32; 64]);

#[cfg(test)]
mod tests {
    use super::*;
    use crate::block_on;

    const PAIRS: usize = 64;
    const ROUNDS: u32 = 48;

    fn pairs() -> Vec<(u32, u32)> {
        let mut out = Vec::new();
        let (mut hi, mut lo): (u32, u32) = (2, 1);
        for _ in 0..45 {
            out.push((hi, lo));
            let next = hi.wrapping_add(lo);
            lo = hi;
            hi = next;
        }
        let mut x: u32 = 12345;
        for i in 45..PAIRS {
            x = x.wrapping_mul(1664525).wrapping_add(1013904223);
            let a = x;
            x = x.wrapping_mul(1664525).wrapping_add(1013904223);
            out.push((a, if i == PAIRS - 1 { 0 } else { x }));
        }
        out
    }

    fn euclid_steps(mut a: u32, mut b: u32) -> (u32, u32) {
        let mut steps = 0;
        while b != 0 {
            (a, b) = (b, a % b);
            steps += 1;
        }
        (a, steps)
    }

    #[test]
    fn matches_reference_euclid_for_every_pair() {
        let result = block_on(run());
        assert_eq!(result.len(), 1);
        for (&(a, b), &got) in pairs().iter().zip(result[0].iter()) {
            assert_eq!(got, euclid_steps(a, b).0, "gcd({a}, {b})");
        }
    }

    #[test]
    fn round_cap_covers_the_worst_case_for_u32() {
        let worst = pairs()
            .iter()
            .map(|&(a, b)| euclid_steps(a, b).1)
            .max()
            .unwrap();
        assert_eq!(pairs()[44], (2_971_215_073, 1_836_311_903));
        assert_eq!(worst, 45);
        assert!(worst < ROUNDS);
        assert_eq!(block_on(run())[0][44], 1);
    }

    #[test]
    fn includes_a_zero_operand_pair() {
        let (a, b) = pairs()[PAIRS - 1];
        assert_eq!(b, 0);
        assert_eq!(block_on(run())[0][PAIRS - 1], a);
    }
}
