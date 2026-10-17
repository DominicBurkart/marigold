async {
    use ::marigold::marigold_impl::*;
    const fn f(x: i32) -> i32 {
  x
}

let mut x = {use ::marigold::marigold_impl::*; ::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new(::marigold::marigold_impl::futures::stream::iter(0..5))};

let mut y = {use ::marigold::marigold_impl::*; ::marigold::marigold_impl::multi_consumer_stream::MultiConsumerStream::new(x.get().map(|v| async move {f(v)}).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2)))};

let returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; y.get()});
let stream_variable_runners_0 = Box::pin({
                let runner_future = Box::pin(x.run());
                ::marigold::marigold_impl::multi_consumer_stream::RunFutureAsStream::new(runner_future)
            });
let stream_variable_runners_1 = Box::pin({
                let runner_future = Box::pin(y.run());
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
stream_variable_runners_1,
]
);let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams
}
