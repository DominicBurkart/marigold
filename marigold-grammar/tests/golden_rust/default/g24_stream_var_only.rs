async {
    use ::marigold::marigold_impl::*;
    let mut x = {use ::marigold::marigold_impl::*; ::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new(::marigold::marigold_impl::futures::stream::iter(0..3))};

let stream_variable_runners_0 = Box::pin({
                let runner_future = Box::pin(x.run());
                ::marigold::marigold_impl::multi_consumer_stream::RunFutureAsStream::new(runner_future)
            });
let streams_array:  Vec<core::pin::Pin<Box<dyn futures::Stream<Item=()>>>> = vec![
stream_variable_runners_0,
]
;let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams.collect::<Vec<()>>().await;
}
