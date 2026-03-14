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
