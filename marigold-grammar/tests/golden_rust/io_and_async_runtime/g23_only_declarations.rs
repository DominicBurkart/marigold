async {
    use ::marigold::marigold_impl::*;
    #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
#[serde(crate = "::marigold::marigold_impl::serde")]enum E {
A,
}
#[allow(dead_code)]
impl E {
    fn __marigold_variants() -> [E; 1] {
        [E::A]
    }
}


            #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
            #[serde(crate = "::marigold::marigold_impl::serde")]
            struct S {
            a: u8,
}

const fn f(x: i32) -> i32 { x }

let streams_array:  Vec<core::pin::Pin<Box<dyn futures::Stream<Item=()>>>> = vec![
]
;let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams.collect::<Vec<()>>().await;
}
