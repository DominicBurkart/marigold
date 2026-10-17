async {
    use ::marigold::marigold_impl::*;
    let streams_array:  Vec<core::pin::Pin<Box<dyn futures::Stream<Item=()>>>> = vec![
]
;let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams.collect::<Vec<()>>().await;
}
