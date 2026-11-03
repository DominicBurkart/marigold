async {
    use ::marigold::marigold_impl::*;
    
            #[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
            #[serde(crate = "::marigold::marigold_impl::serde")]
            struct R {
            a: i32,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, ::marigold::marigold_impl::serde::Serialize, ::marigold::marigold_impl::serde::Deserialize)]
#[serde(crate = "::marigold::marigold_impl::serde")]enum Mood {
#[serde(rename = "😀")]
Happy,
#[serde(rename = "ünï")]
Sad,
}
#[allow(dead_code)]
impl Mood {
    fn __marigold_variants() -> [Mood; 2] {
        [Mood::Happy, Mood::Sad]
    }
}

const fn pick(x: &str) -> bool { /* é */ x == "日本語}" }

let returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; 
           ::marigold::marigold_impl::csv_async::AsyncDeserializer::from_reader(
              ::marigold::marigold_impl::tokio::fs::File::open("dät€.csv")
               .await
               .expect("Marigold could not open file")
               .compat()
           ).into_deserialize::<R>()
           .filter(|r| futures::future::ready(r.is_ok())).map(|r| r.unwrap())});
let non_returning_stream_0 = Box::pin({use ::marigold::marigold_impl::*; {
            if let Some(parent) = ::std::path::Path::new("日本語/ö.csv").parent() {
              ::marigold::marigold_impl::tokio::fs::create_dir_all(parent)
                .await
                .expect("could not create parent directory for output file");
            }

            static WRITER: ::marigold::marigold_impl::once_cell::sync::OnceCell<
              ::marigold::marigold_impl::tokio::sync::Mutex<
                ::marigold::marigold_impl::csv_async::AsyncSerializer<
                  ::marigold::marigold_impl::tokio_util::compat::Compat<
                    ::marigold::marigold_impl::writer::Writer
                  >
                >
              >
            > = ::marigold::marigold_impl::once_cell::sync::OnceCell::new();

            WRITER.set(
              ::marigold::marigold_impl::tokio::sync::Mutex::new(
                ::marigold::marigold_impl::csv_async::AsyncSerializer::from_writer(
                  ::marigold::marigold_impl::writer::Writer::file(
                    ::marigold::marigold_impl::tokio::fs::File::create("日本語/ö.csv")
                       .await
                       .expect("Could not write to file")
                  )
                  .compat_write()
                )
              )
            ).expect("Could not put CSV writer into OnceCell");

            let mut stream_to_write =

           ::marigold::marigold_impl::futures::stream::iter(0..1)
              ;
              stream_to_write.filter_map(
                |v| async move {
                  WRITER
                    .get()
                    .expect("Could not get CSV writer from OnceCell")
                    .lock()
                    .await
                    .serialize(v)
                    .await
                    .expect("could not write record to CSV");
                  None
                }
              ).chain(
                // after the stream is complete, flush the writer.
                ::marigold::marigold_impl::futures::stream::iter(0..1)
                  .filter_map(|_v| async {
                      let mut serializer_guard = WRITER
                        .get() // gets Mutex<...>
                        .expect("Could not get CSV writer from OnceCell")
                        .lock()
                        .await;
                      let mut serializer = std::mem::replace(
                        &mut *serializer_guard,
                        ::marigold::marigold_impl::csv_async::AsyncSerializer::from_writer(
                            ::marigold::marigold_impl::writer::Writer::vector()
                              .compat_write()
                        )
                      );
                      serializer
                        .into_inner()
                        .await
                        .expect("Could not get underlying writer from serializer")
                        .get_mut()
                        .shutdown()
                        .await
                        .expect("Could not shut down underlying writer");
                      None
                  })
              )
          }});

        /// silly function that uses generics to infer the output type (StreamItem) via generics, so that
        /// we can provide the streams as an array of Pin<Box<dyn Stream<Item=StreamItem>>>.
        #[inline(always)]
        fn typed_stream_vec<StreamItem>(v: Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>>) -> Vec<core::pin::Pin<Box<dyn futures::Stream<Item=StreamItem>>>> {
         v
        }
        let streams_array = typed_stream_vec(vec![
returning_stream_0,
non_returning_stream_0,
]
);let mut all_streams = ::marigold::marigold_impl::futures::stream::select_all(streams_array);all_streams
}
