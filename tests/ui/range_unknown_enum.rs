// This file is a trybuild compile-fail fixture.
// It verifies that range(EnumName) where EnumName is not a declared enum
// surfaces as a `compile_error!` diagnostic rather than a proc-macro panic
// or silent codegen failure.
fn main() {
    // NonExistent is not declared in this marigold program, so
    // resolve_enum_range_counts should return an error and the macro
    // should emit compile_error!(...) pointing at the call site.
    let _ = marigold::m!(range(NonExistent).return);
}
