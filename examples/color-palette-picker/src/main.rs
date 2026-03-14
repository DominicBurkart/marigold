use marigold::m;
use marigold::marigold_impl::StreamExt;

use color_palette_picker::compare_contrast;

/// Uses a multithread tokio runtime and tokio-console.
#[cfg(all(feature = "tokio", not(feature = "async-std")))]
#[tokio::main]
async fn main() {
    console_subscriber::init();
    println!(
        "program complete. Best colors: {:?}",
        m!(
            range(0, 255)
            .permutations_with_replacement(3)
            .combinations(5)
            .keep_first_n(20, compare_contrast)
            .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    );
}

/// Uses a multithread async-std runtime.
#[cfg(all(feature = "async-std", not(feature = "tokio")))]
#[async_std::main]
async fn main() {
    println!(
        "program complete. Best colors: {:?}",
        m!(
            range(0, 255)
            .permutations_with_replacement(3)
            .combinations(5)
            .keep_first_n(20, compare_contrast)
            .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    );
}

// When both features are enabled, tokio takes precedence
#[cfg(all(feature = "tokio", feature = "async-std"))]
#[tokio::main]
async fn main() {
    console_subscriber::init();
    println!(
        "program complete. Best colors: {:?}",
        m!(
            range(0, 255)
            .permutations_with_replacement(3)
            .combinations(5)
            .keep_first_n(20, compare_contrast)
            .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    );
}

/// Returns a single future, where all computation occurs in a single thread.
/// This allows Marigold programs to compile with a WASM target.
#[cfg(not(any(feature = "tokio", feature = "async-std")))]
#[tokio::main(flavor = "current_thread")]
async fn main() {
    println!(
        "program complete. Best colors: {:?}",
        m!(
            range(0, 255)
            .permutations_with_replacement(3)
            .combinations(5)
            .keep_first_n(20, compare_contrast)
            .return
        )
        .await
        .collect::<Vec<_>>()
        .await
    );
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn limited_palette() {
        let mod_fifty = |i: u8| i % 50 == 0; // decrease space to search
        assert_eq!(
            m!(
                range(0, 255)
                .filter(mod_fifty)
                .permutations_with_replacement(3)
                .combinations(2)
                .keep_first_n(2, compare_contrast)
                .return
            )
            .await
            .collect::<Vec<_>>()
            .await,
            vec![
                [[0u8, 0, 0], [250u8, 250, 250]],
                [[0u8, 0, 50], [250u8, 250, 250]]
            ]
        );
    }
}

#[cfg(test)]
mod integer_lms_tests {
    use std::cmp::Ordering;

    use color_palette_picker::compare_contrast as prisma_compare;

    const SRGB_LUT: [i64; 256] = [
        0, 304, 607, 911, 1214, 1518, 1821, 2125, 2428, 2732, 3035, 3347, 3677, 4025, 4391, 4777,
        5182, 5605, 6049, 6512, 6995, 7499, 8023, 8568, 9134, 9721, 10330, 10960, 11612, 12286,
        12983, 13702, 14444, 15209, 15996, 16807, 17642, 18500, 19382, 20289, 21219, 22174, 23153,
        24158, 25187, 26241, 27321, 28426, 29557, 30713, 31896, 33105, 34340, 35601, 36889, 38204,
        39546, 40915, 42311, 43735, 45186, 46665, 48172, 49707, 51269, 52861, 54480, 56128, 57805,
        59511, 61246, 63010, 64803, 66626, 68478, 70360, 72272, 74214, 76185, 78187, 80220, 82283,
        84376, 86500, 88656, 90842, 93059, 95307, 97587, 99899, 102242, 104616, 107023, 109462,
        111932, 114435, 116971, 119538, 122139, 124772, 127438, 130136, 132868, 135633, 138432,
        141263, 144128, 147027, 149960, 152926, 155926, 158961, 162029, 165132, 168269, 171441,
        174647, 177888, 181164, 184475, 187821, 191202, 194618, 198069, 201556, 205079, 208637,
        212231, 215861, 219526, 223228, 226966, 230740, 234551, 238398, 242281, 246201, 250158,
        254152, 258183, 262251, 266356, 270498, 274677, 278894, 283149, 287441, 291771, 296138,
        300544, 304987, 309469, 313989, 318547, 323143, 327778, 332452, 337164, 341914, 346704,
        351533, 356400, 361307, 366253, 371238, 376262, 381326, 386429, 391572, 396755, 401978,
        407240, 412543, 417885, 423268, 428690, 434154, 439657, 445201, 450786, 456411, 462077,
        467784, 473531, 479320, 485150, 491021, 496933, 502886, 508881, 514918, 520996, 527115,
        533276, 539479, 545724, 552011, 558340, 564712, 571125, 577580, 584078, 590619, 597202,
        603827, 610496, 617207, 623960, 630757, 637597, 644480, 651406, 658375, 665387, 672443,
        679542, 686685, 693872, 701102, 708376, 715694, 723055, 730461, 737910, 745404, 752942,
        760525, 768151, 775822, 783538, 791298, 799103, 806952, 814847, 822786, 830770, 838799,
        846873, 854993, 863157, 871367, 879622, 887923, 896269, 904661, 913099, 921582, 930111,
        938686, 947307, 955973, 964686, 973445, 982251, 991102, 1000000,
    ];

    fn linearize(v: i32) -> i64 {
        SRGB_LUT[v as usize]
    }

    fn abs_i64(x: i64) -> i64 {
        if x < 0 {
            -x
        } else {
            x
        }
    }

    fn to_lms_l(r: i32, g: i32, b: i32) -> i64 {
        let lr = linearize(r);
        let lg = linearize(g);
        let lb = linearize(b);
        390473 * lr + 549904 * lg + 8902 * lb
    }

    fn to_lms_m(r: i32, g: i32, b: i32) -> i64 {
        let lr = linearize(r);
        let lg = linearize(g);
        let lb = linearize(b);
        70926 * lr + 963107 * lg + 1358 * lb
    }

    fn to_lms_s(r: i32, g: i32, b: i32) -> i64 {
        let lr = linearize(r);
        let lg = linearize(g);
        let lb = linearize(b);
        23143 * lr + 128012 * lg + 936052 * lb
    }

    fn to_luminance(r: i32, g: i32, b: i32) -> i64 {
        let lr = linearize(r);
        let lg = linearize(g);
        let lb = linearize(b);
        212673 * lr + 715152 * lg + 72175 * lb
    }

    fn pair_contrast_achromat(c1: &[i32; 3], c2: &[i32; 3]) -> i64 {
        abs_i64(to_luminance(c1[0], c1[1], c1[2]) - to_luminance(c2[0], c2[1], c2[2]))
    }

    fn pair_contrast_normal(c1: &[i32; 3], c2: &[i32; 3]) -> i64 {
        abs_i64(to_lms_l(c1[0], c1[1], c1[2]) - to_lms_l(c2[0], c2[1], c2[2]))
            + abs_i64(to_lms_m(c1[0], c1[1], c1[2]) - to_lms_m(c2[0], c2[1], c2[2]))
            + abs_i64(to_lms_s(c1[0], c1[1], c1[2]) - to_lms_s(c2[0], c2[1], c2[2]))
    }

    fn pair_contrast_deutan(c1: &[i32; 3], c2: &[i32; 3]) -> i64 {
        abs_i64(to_lms_l(c1[0], c1[1], c1[2]) - to_lms_l(c2[0], c2[1], c2[2]))
            + abs_i64(to_lms_s(c1[0], c1[1], c1[2]) - to_lms_s(c2[0], c2[1], c2[2]))
    }

    fn pair_contrast_protan(c1: &[i32; 3], c2: &[i32; 3]) -> i64 {
        abs_i64(to_lms_m(c1[0], c1[1], c1[2]) - to_lms_m(c2[0], c2[1], c2[2]))
            + abs_i64(to_lms_s(c1[0], c1[1], c1[2]) - to_lms_s(c2[0], c2[1], c2[2]))
    }

    fn pair_contrast_tritan(c1: &[i32; 3], c2: &[i32; 3]) -> i64 {
        abs_i64(to_lms_l(c1[0], c1[1], c1[2]) - to_lms_l(c2[0], c2[1], c2[2]))
            + abs_i64(to_lms_m(c1[0], c1[1], c1[2]) - to_lms_m(c2[0], c2[1], c2[2]))
    }

    fn min_pair_contrast(c1: &[i32; 3], c2: &[i32; 3]) -> i64 {
        let normal = pair_contrast_normal(c1, c2);
        let deutan = pair_contrast_deutan(c1, c2);
        let protan = pair_contrast_protan(c1, c2);
        let tritan = pair_contrast_tritan(c1, c2);
        let achromat = pair_contrast_achromat(c1, c2);
        let mut min = normal;
        if deutan < min {
            min = deutan;
        }
        if protan < min {
            min = protan;
        }
        if tritan < min {
            min = tritan;
        }
        if achromat < min {
            min = achromat;
        }
        min
    }

    fn min_palette_contrast(palette: &[[i32; 3]; 3]) -> i64 {
        let mut min: i64 = i64::MAX;
        let mut i: usize = 0;
        while i < 3 {
            let mut j: usize = i + 1;
            while j < 3 {
                let c = min_pair_contrast(&palette[i], &palette[j]);
                if c < min {
                    min = c;
                }
                j += 1;
            }
            i += 1;
        }
        min
    }

    fn lms_compare(a: &[[i32; 3]; 3], b: &[[i32; 3]; 3]) -> Ordering {
        min_palette_contrast(a).cmp(&min_palette_contrast(b))
    }

    fn to_u8(p: &[[i32; 3]; 3]) -> [[u8; 3]; 3] {
        [
            [p[0][0] as u8, p[0][1] as u8, p[0][2] as u8],
            [p[1][0] as u8, p[1][1] as u8, p[1][2] as u8],
            [p[2][0] as u8, p[2][1] as u8, p[2][2] as u8],
        ]
    }

    fn assert_order_agrees(a: &[[i32; 3]; 3], b: &[[i32; 3]; 3]) {
        let lms_ord = lms_compare(a, b);
        let prisma_ord = prisma_compare::<3>(&to_u8(a), &to_u8(b));
        assert_eq!(
            lms_ord, prisma_ord,
            "ordering disagreement for\n  a={:?}\n  b={:?}\n  integer-LMS={:?}, prisma={:?}",
            a, b, lms_ord, prisma_ord
        );
    }

    #[test]
    fn high_contrast_beats_low_contrast() {
        let high = [[0i32, 0, 0], [255, 255, 255], [128, 0, 255]];
        let low = [[100i32, 100, 100], [120, 120, 120], [110, 110, 110]];
        assert_order_agrees(&low, &high);
    }

    #[test]
    fn black_white_red_vs_grey_trio() {
        let bwr = [[0i32, 0, 0], [255, 255, 255], [255, 0, 0]];
        let grey = [[80i32, 80, 80], [160, 160, 160], [200, 200, 200]];
        assert_order_agrees(&grey, &bwr);
    }

    #[test]
    fn red_green_blue_vs_cyan_magenta_yellow() {
        let rgb = [[255i32, 0, 0], [0, 255, 0], [0, 0, 255]];
        let cmy = [[0i32, 255, 255], [255, 0, 255], [255, 255, 0]];
        assert_order_agrees(&rgb, &cmy);
    }

    #[test]
    fn pure_blue_distinguishable_by_tritan() {
        let blues = [[0i32, 0, 100], [0, 0, 200], [0, 0, 255]];
        let diverse = [[255i32, 0, 0], [0, 255, 0], [0, 0, 255]];
        assert_order_agrees(&blues, &diverse);
    }

    #[test]
    fn pure_red_hard_for_protan() {
        let reds = [[200i32, 0, 0], [255, 0, 0], [128, 0, 0]];
        let diverse = [[255i32, 0, 0], [0, 255, 0], [0, 0, 255]];
        assert_order_agrees(&reds, &diverse);
    }

    #[test]
    fn identical_palettes_compare_equal() {
        let p = [[100i32, 150, 200], [50, 50, 200], [200, 100, 50]];
        let lms_ord = lms_compare(&p, &p);
        let prisma_ord = prisma_compare::<3>(&to_u8(&p), &to_u8(&p));
        assert_eq!(lms_ord, Ordering::Equal);
        assert_eq!(prisma_ord, Ordering::Equal);
    }

    #[test]
    fn cvd_sensitive_red_green_pair() {
        let red_green = [[255i32, 0, 0], [0, 255, 0], [128, 128, 128]];
        let red_blue = [[255i32, 0, 0], [0, 0, 255], [128, 128, 128]];
        assert_order_agrees(&red_green, &red_blue);
    }

    #[test]
    fn near_identical_colors_low_contrast() {
        let near = [[100i32, 100, 100], [101, 101, 101], [50, 50, 50]];
        let spread = [[0i32, 0, 0], [128, 128, 128], [255, 255, 255]];
        assert_order_agrees(&near, &spread);
    }

    #[test]
    fn dark_palette_vs_light_palette() {
        let dark = [[10i32, 10, 10], [20, 20, 20], [30, 30, 30]];
        let light = [[200i32, 200, 200], [220, 220, 220], [240, 240, 240]];
        let diverse = [[0i32, 0, 0], [128, 0, 255], [255, 128, 0]];
        assert_order_agrees(&dark, &diverse);
        assert_order_agrees(&light, &diverse);
    }

    #[test]
    fn orange_purple_teal_is_accessible() {
        let accessible = [[255i32, 102, 0], [128, 0, 128], [0, 128, 128]];
        let similar = [[200i32, 150, 100], [210, 160, 110], [190, 140, 90]];
        assert_order_agrees(&similar, &accessible);
    }

    #[tokio::test]
    async fn small_range_pipeline_returns_five_palettes() {
        use marigold::m;
        use marigold::marigold_impl::StreamExt;

        let results = m!(
            range(0, 4)
            .permutations_with_replacement(3)
            .combinations(3)
            .keep_first_n(5, lms_compare)
            .return
        )
        .await
        .collect::<Vec<_>>()
        .await;

        assert_eq!(results.len(), 5);
        for palette in &results {
            for color in palette {
                for channel in color {
                    assert!(*channel >= 0 && *channel < 4);
                }
            }
        }
    }
}
