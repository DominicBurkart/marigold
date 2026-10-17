async {
    use ::marigold::marigold_impl::*;
    let returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; ::marigold::marigold_impl::futures::stream::iter(0..=10).permutations(2).await.map(|v| <[_; 2]>::try_from(v).unwrap()).combinations(2).await.map(|v| <[_; 2]>::try_from(v).unwrap()).permutations_with_replacement(2).await.map(|v| <[_; 2]>::try_from(v).unwrap()).keep_first_n(3, cmp).await.marifold(0, |acc, x| futures::future::ready(add(acc, x))).await.map(|v| async move {a(v)}).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2)).map(|v| async move { if b(v) { Some(v) } else { None } }).buffered(std::cmp::max(2 * (::marigold::marigold_impl::num_cpus::get() - 1), 2)).filter_map(|v| v).filter_map(|v| async move { c(v) })});

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
