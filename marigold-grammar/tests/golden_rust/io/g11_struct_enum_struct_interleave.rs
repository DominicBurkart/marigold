async {
    use ::marigold::marigold_impl::*;
    
            #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
            #[serde(crate = "::marigold::marigold_impl::serde")]
            struct B {
            a: u8,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
#[serde(crate = "::marigold::marigold_impl::serde")]enum A {
X,
}
#[allow(dead_code)]
impl A {
    fn __marigold_variants() -> [A; 1] {
        [A::X]
    }
}


            #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
            #[serde(crate = "::marigold::marigold_impl::serde")]
            struct C {
            a: u8,
}

let streams_array:  Vec<core::pin::Pin<Box<dyn futures::Stream<Item=()>>>> = vec![
]
;let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams.collect::<Vec<()>>().await;
}
