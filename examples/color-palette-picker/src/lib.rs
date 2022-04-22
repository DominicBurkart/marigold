use once_cell::sync::Lazy;
use prisma::color_space::named::SRgb;
use prisma::color_space::ConvertToXyz;
use prisma::encoding::EncodableColor;
use prisma::{lms::LmsCam2002, FromColor, Rgb};

static SRGB: Lazy<SRgb<f64>> = Lazy::new(SRgb::new);

/// Returns the minimal contrast across normative vision as
/// well as red-green, blue-yellow, and total color blindness.
fn min_contrast<T>(_colors: Vec<LmsCam2002<T>>) -> u32 {
    10 // unimplemented
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
    palette_1_min_contrast.cmp(&palette_2_min_contrast)
}
