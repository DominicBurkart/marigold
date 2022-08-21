use once_cell::sync::Lazy;
use prisma::color_space::named::SRgb;
use prisma::color_space::ConvertToXyz;
use prisma::encoding::EncodableColor;
use prisma::{lms::LmsCam2002, FromColor, Rgb};

static SRGB: Lazy<SRgb<f64>> = Lazy::new(SRgb::new);

/// Returns the minimal contrast across normative vision, deuteranomaly, protanomaly, and
/// tritanomaly.
fn min_contrast(colors: Vec<LmsCam2002<f64>>) -> f64 {
    let mut min = f64::MAX;
    for i1 in 0..colors.len() {
        for i2 in 0..colors.len() {
            if i1 != i2 {
                let color1 = colors[i1];
                let color2 = colors[i2];

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
            }
        }
    }
    min
}

/// Uses [prisma](https://crates.io/crates/prisma) to convert u8 RGBs into
/// [CIECAM02](https://en.wikipedia.org/wiki/CIECAM02) values, which are then
/// tested for contrast.
#[allow(clippy::ptr_arg)]
pub fn compare_contrast(palette1: &Vec<Vec<u8>>, palette2: &Vec<Vec<u8>>) -> std::cmp::Ordering {
    fn to_lms(v: &[u8]) -> LmsCam2002<f64> {
        let rgb = Rgb::new(v[0], v[1], v[2])
            .color_cast::<f64>()
            .srgb_encoded();
        let xyz = SRGB.convert_to_xyz(&rgb);
        LmsCam2002::from_color(&xyz)
    }

    let palette_1_min_contrast = min_contrast(cute::c![
        to_lms(v),
        for v in palette1.iter()
    ]);
    let palette_2_min_contrast = min_contrast(cute::c![
        to_lms(v),
        for v in palette2.iter()
    ]);
    palette_1_min_contrast.total_cmp(&palette_2_min_contrast)
}
