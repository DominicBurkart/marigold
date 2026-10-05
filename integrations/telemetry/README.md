# Marigold telemetry

Marigold programs can export per-node counts to OpenTelemetry, and the
`marigold-lsp` language server and MCP server can read those counts back
from Datadog and show them next to the static complexity of each stream.

This page covers both halves: exporting from a program, and reading in
the language server.

## What is measured

Every input, stream function and output of every stream expression gets a
stable node id. With instrumentation on, the generated code records these
counters, tagged only with `marigold.program` and `marigold.node_id`:

- `marigold.node.items_in`: items that reached the node.
- `marigold.node.items_out`: items the node produced.
- `marigold.node.runs`: times the node's stream was started.
- `marigold.node.errors.item`: `Err` items seen at the node.
- `marigold.node.errors.abort`: runs of the node aborted by a panic.

A latency histogram, `marigold.node.duration`, is recorded as well. The
language server does not read it yet.

Spans additionally carry `marigold.file` and the byte range of the node.
The range is approximate: for the `m!` macro it is an offset into the
stringified macro body, not into your Rust source file, so do not use it to
locate code. Node ids are exact.

## Instrumenting a program

Two features of the `marigold` crate control telemetry:

- `otel`: instrumentation only. The generated code records to the global
  OpenTelemetry providers, and the crate depends on the OpenTelemetry API
  and SDK but on no exporter, HTTP client or async runtime. It works with
  tokio, async-std or no runtime at all. Use it when you install your own
  provider or exporter with `opentelemetry_sdk`, or when you only want the
  instrumentation compiled in.
- `otel-otlp`: implies `otel` and adds `marigold::telemetry::init`, an OTLP
  exporter over HTTP with protobuf bodies and a TLS-capable client
  (rustls), so `https` endpoints work. It also adds a TLS stack to the
  build, which includes `aws-lc-sys` and therefore needs a C compiler and
  CMake at build time.

```toml
[dependencies]
marigold = { version = "0.2", features = ["otel-otlp"] }
```

Without either feature the generated code is byte-identical to a build that
never heard of telemetry, so there is no overhead. With a feature on but no
provider installed, the instrumentation records nothing.

With `otel-otlp`, install the exporter once, early in `main`, and keep the
guard alive until the program should stop exporting:

```rust
let _telemetry = marigold::telemetry::init().expect("telemetry");
```

Marigold never calls `init` on its own. Dropping the guard flushes and
shuts the exporters down.

## Exporter configuration

With `otel-otlp`, the exporter speaks OTLP over HTTP with protobuf bodies
and the OpenTelemetry SDK reads its configuration from the standard
variables:

- `OTEL_EXPORTER_OTLP_ENDPOINT`: where to send, for example
  `http://localhost:4318`.
- `OTEL_EXPORTER_OTLP_HEADERS`: extra headers such as an API key.
- `OTEL_SERVICE_NAME`: becomes `marigold.program`. When unset, the crate
  or file name captured at code generation is used.
- `OTEL_RESOURCE_ATTRIBUTES`: extra resource attributes such as
  `deployment.environment=prod`.
- `OTEL_METRIC_EXPORT_INTERVAL`: export period in milliseconds.

`https` endpoints work. Sending straight to a vendor endpoint is possible,
but a local Datadog Agent or OpenTelemetry Collector keeps API keys out of
your program and is what the Datadog section below assumes.

## Sending to Datadog through the Agent

Run the Datadog Agent with its OTLP receiver enabled on the HTTP port, for
example with the environment variable
`DD_OTLP_CONFIG_RECEIVER_PROTOCOLS_HTTP_ENDPOINT=0.0.0.0:4318` and the
usual `DD_API_KEY` and `DD_SITE` of the Agent. Then start the program with:

```sh
OTEL_SERVICE_NAME=my-pipeline \
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 \
./my-pipeline
```

The metrics then appear in Datadog under their dotted names, with
`marigold.program` and `marigold.node_id` as tags.

## Node ids and call sites

Ids are hashed from the program name (the crate name at code generation;
`OTEL_SERVICE_NAME` changes only the `marigold.program` tag), the variable
name or the normalised expression text, and the position in the chain. The
normalisation drops whitespace outside string literals, so the ids in
generated code equal the ids the language server computes from the same
program in a `.marigold` file.

Two `m!` invocations in one crate can contain the same variable name or an
identical unnamed expression, which would give them the same ids. The macro
detects this: the first invocation keeps the plain ids and each later one is
given a discriminator derived from its call-site span, so their metrics stay
separate. The consequences:

- A discriminated invocation has ids the language server cannot compute,
  because the discriminator depends on the Rust call site. Its metrics are
  correct but are not annotated in the editor. Give such programs distinct
  variable names, or move them into separate `.marigold` files, to keep
  them annotated.
- Which invocation counts as "first" follows compiler expansion order, and
  the discriminator follows the byte offsets of the call site. Both are
  stable for a given source file, but editing earlier code can shift the
  offsets and with them the ids of the discriminated invocations.
- The call-site span is read from the compiler's debug rendering, since
  its structured form is unstable. If a compiler does not provide offsets,
  invocations in the same crate that share ids would merge again. The same
  is true for one macro-generated invocation expanded twice from the same
  `macro_rules!` body, which has a single call site.

## Reading telemetry in the language server

Build the language server with the Datadog source. Telemetry is off by
default and absent from the default build:

```sh
cargo install marigold-lsp --features datadog
```

Configure it entirely through the environment of the editor or agent
process that launches `marigold-lsp`:

- `MARIGOLD_TELEMETRY=datadog` turns the feature on. Without it, nothing
  changes: capabilities and the MCP tool list are identical to a build
  without telemetry.
- `DD_API_KEY` and `DD_APP_KEY`: an API key and an application key allowed
  to read metrics.
- `DD_SITE`: optional, defaults to `datadoghq.com`.
- `MARIGOLD_TELEMETRY_WINDOW`: optional lookback such as `24h`, `30m` or
  `7d`. The default is `24h`.
- `OTEL_SERVICE_NAME`: optional. It must match the program's value so the
  right series are queried. Without it, the file name without extension
  is used, for example `main` for `main.marigold`.

The keys are read only from the environment. They are sent only to
`https://api.<DD_SITE>` as request headers, and never appear in logs,
error messages, `Debug` output, hover, hints or MCP results. Plain HTTP
and redirects are refused, each request times out after five seconds, and
responses larger than 4 MiB are rejected.

When telemetry is on you get:

- Hover: an extra line such as
  `observed: 40000 inputs · 3 runs · 2 errors (last 24h)`.
- Inlay hints, shown at the end of each annotated node, for example
  `40000 in · 3 runs · 2 err`. The `inlayHintProvider` capability is
  advertised only when telemetry is on.
- The read-only MCP tool `marigold_telemetry` with a `path` argument,
  returning the same counts per node as JSON.

Telemetry does not change diagnostics. Undefined stream variables are
reported as warnings, and undefined functions and structs as information,
because they can be Rust items in scope inside `m!()`.

A failing or unreachable backend never affects diagnostics or hover: the
annotations are simply absent. The MCP tool reports the failure instead.
Results are cached for 30 seconds per open document, and a lookup blocks
the request that triggered it for at most a few seconds.

## Cardinality

Metrics carry exactly two tags, so the number of series per metric is the
number of nodes in a program, times the number of distinct program names.
Keep `OTEL_SERVICE_NAME` stable across deploys; putting a version or a
hostname in it multiplies series. The file and range are deliberately kept
off metric tags. Datadog bills custom metrics by series count, so check
the volume with a small program first.

## Privacy

Only counts and node ids are exported. No item values, no input data and
no source text ever leave the program, and the language server sends only
metric names, the program name and node ids in its queries. Source text is
never sent to Datadog.

## Stale annotations

Node ids survive whitespace edits and edits to other expressions, so a
count can outlive the code it was recorded against. An annotation whose
recorded content hash differs from the current node text is marked
`(stale)`. The Datadog source cannot do this yet: the exported metrics do
not carry a content hash, so Datadog annotations are never marked stale.
The check is implemented and tested for sources that do report a hash.

## Unverified

Nothing here has been run against a live Datadog account.

- The Datadog metric and tag names are assumed to be the dotted OTLP
  names unchanged, and tag values are assumed to be lowercased with
  unusual characters replaced by underscores. A program name that does
  not match may silently yield no annotations.
- The OTLP receiver settings of the Agent are quoted from memory, not
  checked against the current Agent documentation.
- Counter temporality handling in the Agent, and the `.as_count()` sums
  the language server asks for, are assumed to give totals over the window.
- The live test, `live_datadog_query_succeeds`, is ignored by default. Run
  it with `DD_API_KEY` and `DD_APP_KEY` set:

```sh
cargo test -p marigold-lsp --features datadog -- --ignored
```
