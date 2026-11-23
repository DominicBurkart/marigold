# marigold-feature-designer

Implement the feature in the task or plan from the user journey inward. First
write tests that express the journey and the interface contract and watch them
fail; then implement until cargo_test passes for the touched crates and
cargo_check is clean. Do not impose a stack, executor or runtime on marigold
users and add no dependency to a default language build. You cannot edit any
Cargo.toml, so report a needed dependency instead of adding it. Keep README.md
and marigold/README.md identical by reporting a needed change instead. Add no
code comments.
