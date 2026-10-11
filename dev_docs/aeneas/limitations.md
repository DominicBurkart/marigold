# Aeneas limitations

This file is the authoritative list of Rust features Aeneas does not
support. Cite it from other docs instead of re-enumerating. If you are
considering adding formal verification to a piece of code, check this list
first.

## Unsupported Rust features

- `async` / `await` and any `Future`-returning function.
- `tokio::*`, `std::thread`, and any other runtime-specific concurrency
  APIs.
- `Pin<P>` in any form, including `Pin<Box<dyn Future>>`.
- `Box<dyn Trait>` and any other `dyn Trait` trait object.
- `unsafe` blocks, raw pointers (`*const T`, `*mut T`), and `transmute`.
- FFI: `extern "C"` functions and C bindings.
- `Rc<T>` and `Arc<T>` when used to build cycles. Forward-only shared
  references may work on a case-by-case basis but are not guaranteed.
- Complex higher-ranked trait bounds (HRTBs) such as `for<'a> F: Fn(&'a T)`.
- Macros that expand to any of the above unsupported features.
- Complex generic bounds, generic associated types (GATs), and const
  generics that involve arithmetic in the type.

## Rule of thumb

If the function compiles under `#![forbid(unsafe_code)]`, is fully
synchronous, takes owned or simple borrowed arguments, and its signature
would be readable to a first-year functional-programming student, it is
probably in-scope for Aeneas. If not, it probably is not.

When in doubt, consult the Aeneas upstream README and this project's
[`agent_workflow.md`](agent_workflow.md) procedure 1.
