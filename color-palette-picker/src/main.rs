#[macro_use(c)]
extern crate cute;
use marigold::m;
use once_cell::sync::Lazy;
use prisma::color_space::named::SRgb;
use prisma::color_space::ConvertToXyz;
use prisma::encoding::EncodableColor;
use prisma::{lms::LmsCam2002, FromColor, Rgb};

static SRGB: Lazy<SRgb<f64>> = Lazy::new(SRgb::new);

/// Returns the minimal contrast across normative triconic vision as
/// well as red-green, blue-yellow, and total color blindness.
fn min_contrast<T>(_colors: Vec<LmsCam2002<T>>) -> u32 {
    10 // unimplemented
}

/// Uses [prisma](https://crates.io/crates/prisma) to convert u8 RGBs into
/// [CIECAM02](https://en.wikipedia.org/wiki/CIECAM02) values, which are then
/// tested for contrast.
#[allow(clippy::ptr_arg)]
fn compare_contrast(palette1: &Vec<Vec<u8>>, palette2: &Vec<Vec<u8>>) -> std::cmp::Ordering {
    fn to_lms(v: &[u8]) -> LmsCam2002<f64> {
        let rgb = Rgb::new(v[0], v[1], v[2])
            .color_cast::<f64>()
            .srgb_encoded();
        let xyz = SRGB.convert_to_xyz(&rgb);
        LmsCam2002::from_color(&xyz)
    }

    let palette_1_min_contrast = min_contrast(c![
        to_lms(v),
        for v in palette1.iter()
    ]);
    let palette_2_min_contrast = min_contrast(c![
        to_lms(v),
        for v in palette2.iter()
    ]);
    palette_1_min_contrast.cmp(&palette_2_min_contrast)
}

#[tokio::main]
async fn main() {
    println!(
        "program complete. Best colors: {:?}",
        m!(
            range(0, 255)
            .permutations_with_replacement(3)
            .combinations(2)
            .keep_first_n(20, compare_contrast)
            .to_vec()
            .return
        )
        .await
    );
}
