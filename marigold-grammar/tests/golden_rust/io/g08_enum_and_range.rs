async {
    use ::marigold::marigold_impl::*;
    #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
#[serde(crate = "::marigold::marigold_impl::serde")]
#[serde(try_from="String")]
enum E {
A,
#[serde(rename = "b")]
B,
#[serde(skip_deserializing, rename = "Z")]
Z,
}

                impl TryFrom<String> for E {
                    type Error = ::marigold::marigold_impl::arrayvec::CapacityError<char>;

                    fn try_from(s: String) -> Result<Self, Self::Error> {
                        use E::*;

                        Ok(match s.as_str() {
                            "A" => A,
"b" => B,

                            _ => Z
                        })
                    }
                }

                
#[allow(dead_code)]
impl E {
    fn __marigold_variants() -> [E; 3] {
        [E::A, E::B, E::Z]
    }
}

let returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; ::marigold::marigold_impl::futures::stream::iter(E::__marigold_variants())});

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
