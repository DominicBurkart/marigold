async {
    use ::marigold::marigold_impl::*;
    #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
#[serde(crate = "::marigold::marigold_impl::serde")]enum Color {
Red,
Green,
}
#[allow(dead_code)]
impl Color {
    fn __marigold_variants() -> [Color; 2] {
        [Color::Red, Color::Green]
    }
}


            #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
            #[serde(crate = "::marigold::marigold_impl::serde")]
            struct P {
            c: u8,
}
impl P {
    pub const C_MIN: u8 = 0;
    pub const C_MAX: u8 = 2;
    pub const C_CARDINALITY: u8 = 3;
}

let mut x = {use ::marigold::marigold_impl::*; ::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new(::marigold::marigold_impl::futures::stream::iter(Color::__marigold_variants()))};

let returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; x.get()});
let stream_variable_runners_0 = Box::pin({
                let runner_future = Box::pin(x.run());
                ::marigold::marigold_impl::multi_consumer_stream::RunFutureAsStream::new(runner_future)
            });

        /// silly function that uses generics to infer the output type (StreamItem) via generics, so that
        /// we can provide the streams as an array of Pin<Box<dyn Stream<Item=StreamItem>>>.
        #[inline(always)]
        fn typed_stream_vec<StreamItem>(v: Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>>) -> Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>> {
         v
        }
        let streams_array = typed_stream_vec(vec![
returning_stream_0,
stream_variable_runners_0,
]
);let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams
}
