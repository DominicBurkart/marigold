#[cfg(test)]
mod lut_gen {
    #[test]
    #[ignore]
    fn print_srgb_lut_and_matrix() {
        let lut: Vec<i64> = (0u32..=255)
            .map(|v| {
                let linear = if v <= 10 {
                    v as f64 / (255.0 * 12.92)
                } else {
                    ((v as f64 / 255.0 + 0.055) / 1.055).powf(2.4)
                };
                (linear * 1_000_000.0).round() as i64
            })
            .collect();

        print!("const SRGB_LUT: [i64; 256] = [");
        for (i, v) in lut.iter().enumerate() {
            if i % 8 == 0 {
                print!("\n    ");
            }
            print!("{}", v);
            if i < 255 {
                print!(", ");
            }
        }
        println!("\n];");

        let srgb_to_xyz: [[f64; 3]; 3] = [
            [0.4124564, 0.3575761, 0.1804375],
            [0.2126729, 0.7151522, 0.0721750],
            [0.0193339, 0.1191920, 0.9503041],
        ];
        let xyz_to_lms: [[f64; 3]; 3] = [
            [0.7328, 0.4296, -0.1624],
            [-0.7036, 1.6975, 0.0061],
            [0.0030, 0.0136, 0.9834],
        ];

        let mut combined = [[0f64; 3]; 3];
        for i in 0..3 {
            for j in 0..3 {
                for k in 0..3 {
                    combined[i][j] += xyz_to_lms[i][k] * srgb_to_xyz[k][j];
                }
            }
        }
        println!("Combined sRGB_linear -> LMS matrix (×1_000_000):");
        let names = ["L", "M", "S"];
        for (i, row) in combined.iter().enumerate() {
            println!(
                "  {} = {}*R + {}*G + {}*B",
                names[i],
                (row[0] * 1_000_000.0).round() as i64,
                (row[1] * 1_000_000.0).round() as i64,
                (row[2] * 1_000_000.0).round() as i64,
            );
        }
    }
}

use once_cell::sync::Lazy;
use prisma::color_space::named::SRgb;
use prisma::color_space::ConvertToXyz;
use prisma::encoding::EncodableColor;
use prisma::{lms::LmsCam2002, FromColor, Rgb};

static SRGB: Lazy<SRgb<f64>> = Lazy::new(SRgb::new);

fn to_lms(v: &[u8; 3]) -> LmsCam2002<f64> {
    let rgb = Rgb::new(v[0], v[1], v[2])
        .color_cast::<f64>()
        .srgb_encoded();
    let xyz = SRGB.convert_to_xyz(&rgb);
    LmsCam2002::from_color(&xyz)
}

fn to_luminance(v: &[u8; 3]) -> f64 {
    let rgb = Rgb::new(v[0], v[1], v[2])
        .color_cast::<f64>()
        .srgb_encoded();
    SRGB.convert_to_xyz(&rgb).y()
}

/// Returns the minimal contrast across normative vision, deuteranomaly, protanomaly,
/// tritanomaly, and achromatopsia (monochromacy).
fn min_contrast<const N: usize>(colors: &[[u8; 3]; N]) -> f64 {
    let mut min = f64::MAX;
    for i1 in 0..N {
        for i2 in 0..N {
            if i1 != i2 {
                let color1 = to_lms(&colors[i1]);
                let color2 = to_lms(&colors[i2]);

                let rgb_contrast = (color1.l() - color2.l()).abs()
                    + (color1.m() - color2.m()).abs()
                    + (color1.s() - color2.s()).abs();
                if rgb_contrast < min {
                    min = rgb_contrast;
                }

                let deuteranomaly_contrast =
                    (color1.l() - color2.l()).abs() + (color1.s() - color2.s()).abs();
                if deuteranomaly_contrast < min {
                    min = deuteranomaly_contrast;
                }

                let protanomaly_contrast =
                    (color1.m() - color2.m()).abs() + (color1.s() - color2.s()).abs();
                if protanomaly_contrast < min {
                    min = protanomaly_contrast;
                }

                let tritanomaly_contrast =
                    (color1.l() - color2.l()).abs() + (color1.m() - color2.m()).abs();
                if tritanomaly_contrast < min {
                    min = tritanomaly_contrast;
                }

                let achromat_contrast =
                    (to_luminance(&colors[i1]) - to_luminance(&colors[i2])).abs();
                if achromat_contrast < min {
                    min = achromat_contrast;
                }
            }
        }
    }
    min
}

/// Uses [prisma](https://crates.io/crates/prisma) to convert u8 RGBs into
/// [CIECAM02](https://en.wikipedia.org/wiki/CIECAM02) values, which are then
/// tested for contrast.
pub fn compare_contrast<const N: usize>(
    palette1: &[[u8; 3]; N],
    palette2: &[[u8; 3]; N],
) -> std::cmp::Ordering {
    min_contrast(palette1).total_cmp(&min_contrast(palette2))
}

pub async fn geometric_palettes() -> Vec<[[i32; 3]; 3]> {
    use marigold::m;
    use marigold::marigold_impl::StreamExt;

    m!(
        fn linearize(v: i32) -> i64 {
            const TABLE: [i64; 256] = [
                0, 304, 607, 911, 1214, 1518, 1821, 2125,
                2428, 2732, 3035, 3347, 3677, 4025, 4391, 4777,
                5182, 5605, 6049, 6512, 6995, 7499, 8023, 8568,
                9134, 9721, 10330, 10960, 11612, 12286, 12983, 13702,
                14444, 15209, 15996, 16807, 17642, 18500, 19382, 20289,
                21219, 22174, 23153, 24158, 25187, 26241, 27321, 28426,
                29557, 30713, 31896, 33105, 34340, 35601, 36889, 38204,
                39546, 40915, 42311, 43735, 45186, 46665, 48172, 49707,
                51269, 52861, 54480, 56128, 57805, 59511, 61246, 63010,
                64803, 66626, 68478, 70360, 72272, 74214, 76185, 78187,
                80220, 82283, 84376, 86500, 88656, 90842, 93059, 95307,
                97587, 99899, 102242, 104616, 107023, 109462, 111932, 114435,
                116971, 119538, 122139, 124772, 127438, 130136, 132868, 135633,
                138432, 141263, 144128, 147027, 149960, 152926, 155926, 158961,
                162029, 165132, 168269, 171441, 174647, 177888, 181164, 184475,
                187821, 191202, 194618, 198069, 201556, 205079, 208637, 212231,
                215861, 219526, 223228, 226966, 230740, 234551, 238398, 242281,
                246201, 250158, 254152, 258183, 262251, 266356, 270498, 274677,
                278894, 283149, 287441, 291771, 296138, 300544, 304987, 309469,
                313989, 318547, 323143, 327778, 332452, 337164, 341914, 346704,
                351533, 356400, 361307, 366253, 371238, 376262, 381326, 386429,
                391572, 396755, 401978, 407240, 412543, 417885, 423268, 428690,
                434154, 439657, 445201, 450786, 456411, 462077, 467784, 473531,
                479320, 485150, 491021, 496933, 502886, 508881, 514918, 520996,
                527115, 533276, 539479, 545724, 552011, 558340, 564712, 571125,
                577580, 584078, 590619, 597202, 603827, 610496, 617207, 623960,
                630757, 637597, 644480, 651406, 658375, 665387, 672443, 679542,
                686685, 693872, 701102, 708376, 715694, 723055, 730461, 737910,
                745404, 752942, 760525, 768151, 775822, 783538, 791298, 799103,
                806952, 814847, 822786, 830770, 838799, 846873, 854993, 863157,
                871367, 879622, 887923, 896269, 904661, 913099, 921582, 930111,
                938686, 947307, 955973, 964686, 973445, 982251, 991102, 1000000
            ];
            TABLE[v as usize]
        }

        fn abs_i64(x: i64) -> i64 {
            if x < 0 { -x } else { x }
        }

        fn to_lms_l(r: i32, g: i32, b: i32) -> i64 {
            let lr = linearize(r); let lg = linearize(g); let lb = linearize(b);
            390473 * lr + 549904 * lg + 8902 * lb
        }

        fn to_lms_m(r: i32, g: i32, b: i32) -> i64 {
            let lr = linearize(r); let lg = linearize(g); let lb = linearize(b);
            70926 * lr + 963107 * lg + 1358 * lb
        }

        fn to_lms_s(r: i32, g: i32, b: i32) -> i64 {
            let lr = linearize(r); let lg = linearize(g); let lb = linearize(b);
            23143 * lr + 128012 * lg + 936052 * lb
        }

        fn to_luminance(r: i32, g: i32, b: i32) -> i64 {
            let lr = linearize(r); let lg = linearize(g); let lb = linearize(b);
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
            if deutan < min { min = deutan; }
            if protan < min { min = protan; }
            if tritan < min { min = tritan; }
            if achromat < min { min = achromat; }
            min
        }

        fn min_palette_contrast(palette: &[[i32; 3]; 3]) -> i64 {
            let mut min: i64 = i64::MAX;
            let mut i: usize = 0;
            while i < 3 {
                let mut j: usize = i + 1;
                while j < 3 {
                    let c = min_pair_contrast(&palette[i], &palette[j]);
                    if c < min { min = c; }
                    j = j + 1;
                }
                i = i + 1;
            }
            min
        }

        fn wheel_rgb(hue: i32, ring: i32) -> [i32; 3] {
            const HUES: [[i32; 3]; 12] = [
                [255, 0, 0], [255, 128, 0], [255, 255, 0], [128, 255, 0],
                [0, 255, 0], [0, 255, 128], [0, 255, 255], [0, 128, 255],
                [0, 0, 255], [128, 0, 255], [255, 0, 255], [255, 0, 128]
            ];
            let base = HUES[hue as usize];
            let mut out = [0i32; 3];
            let mut i: usize = 0;
            while i < 3 {
                let c = base[i];
                out[i] = if ring == 0 { c + (255 - c) * 2 / 3 }
                    else if ring == 1 { c }
                    else if ring == 2 { c * 2 / 3 }
                    else { c / 3 };
                i += 1;
            }
            out
        }

        fn to_palette(p: [i32; 4]) -> [[i32; 3]; 3] {
            [wheel_rgb(p[0], p[1]), wheel_rgb(p[0] + 4, p[2]), wheel_rgb(p[0] + 8, p[3])]
        }

        fn compare_polygons(a: &[i32; 4], b: &[i32; 4]) -> Ordering {
            let ma = min_palette_contrast(&to_palette(*a));
            let mb = min_palette_contrast(&to_palette(*b));
            if ma < mb { Ordering::Less }
            else if ma > mb { Ordering::Greater }
            else { Ordering::Equal }
        }

        range(0, 4)
            .permutations_with_replacement(4)
            .keep_first_n(5, compare_polygons)
            .map(to_palette)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await
}
