use marigold::m;
use marigold::marigold_impl::StreamExt;

async fn bounded_int_basic() {
    let values = m!(
        struct Sensor {
            reading: boundedInt(-40, 85)
        }

        fn to_sensor(i: i32) -> Sensor {
            Sensor { reading: i as i8 }
        }

        fn get_reading(s: Sensor) -> i8 {
            s.reading
        }

        range(0, 3)
            .map(to_sensor)
            .map(get_reading)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_eq!(values, vec![0i8, 1, 2]);
}

async fn bounded_uint_basic() {
    let values = m!(
        struct Pixel {
            brightness: boundedUint(0, 255)
        }

        fn to_pixel(i: i32) -> Pixel {
            Pixel { brightness: i as u8 }
        }

        fn get_brightness(p: Pixel) -> u8 {
            p.brightness
        }

        range(0, 3)
            .map(to_pixel)
            .map(get_brightness)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_eq!(values, vec![0u8, 1, 2]);
}

async fn bounded_with_enum_reference() {
    let values = m!(
        enum Direction {
            North = "n",
            South = "s",
            East = "e",
            West = "w",
        }

        struct Compass {
            heading: boundedUint(0, Direction.len())
        }

        fn to_compass(i: i32) -> Compass {
            Compass { heading: i as u8 }
        }

        fn get_heading(c: Compass) -> u8 {
            c.heading
        }

        range(0, 4)
            .map(to_compass)
            .map(get_heading)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_eq!(values, vec![0u8, 1, 2, 3]);
}

async fn bounded_with_arithmetic_bounds() {
    let values = m!(
        enum Color {
            Red = "r",
            Green = "g",
            Blue = "b",
        }

        struct Palette {
            index: boundedUint(0, Color.len() - 1)
        }

        fn to_palette(i: i32) -> Palette {
            Palette { index: i as u8 }
        }

        fn get_index(p: Palette) -> u8 {
            p.index
        }

        range(0, 2)
            .map(to_palette)
            .map(get_index)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_eq!(values, vec![0u8, 1]);
}

async fn bounded_multiple_fields() {
    let values = m!(
        struct Point {
            x: boundedInt(-1000, 1000),
            y: boundedInt(-1000, 1000)
        }

        fn to_point(i: i32) -> Point {
            Point { x: i as i16, y: (i * 2) as i16 }
        }

        fn sum_coords(p: Point) -> i16 {
            p.x + p.y
        }

        range(0, 3)
            .map(to_point)
            .map(sum_coords)
            .return
    )
    .await
    .collect::<Vec<_>>()
    .await;

    assert_eq!(values, vec![0i16, 3, 6]);
}

async fn run_all() {
    bounded_int_basic().await;
    bounded_uint_basic().await;
    bounded_with_enum_reference().await;
    bounded_with_arithmetic_bounds().await;
    bounded_multiple_fields().await;
    println!("all bounded type examples passed");
}

#[tokio::main(flavor = "current_thread")]
async fn main() {
    run_all().await;
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn bounded_types_happy_paths() {
        run_all().await;
    }

    #[test]
    fn compile_fail() {
        let t = trybuild::TestCases::new();
        t.compile_fail("compile_fail/*.rs");
    }
}
