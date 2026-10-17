async {
    use ::marigold::marigold_impl::*;
    
            #[derive(Copy, Clone, Debug, Eq, PartialEq)]
            struct P {
            v: u8,
w: u8,
}
impl P {
    pub const V_MIN: u8 = 0;
    pub const V_MAX: u8 = 255;
    pub const V_CARDINALITY: u16 = 256;
    pub const W_MIN: u8 = 1;
    pub const W_MAX: u8 = 10;
    pub const W_CARDINALITY: u8 = 10;
}

let returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; ::marigold::marigold_impl::futures::stream::iter(0..1)});

        /// silly function that uses generics to infer the output type (StreamItem) via generics, so that
        /// we can provide the streams as an array of Pin<Box<dyn Stream<Item=StreamItem>>>.
        #[inline(always)]
        fn typed_stream_vec<StreamItem>(v: Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>>) -> Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>> {
         v
        }
        let streams_array = typed_stream_vec(vec![
returning_stream_0,
]
);let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams
}
