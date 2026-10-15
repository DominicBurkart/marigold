#![cfg(feature = "otel")]

use marigold_impl::telemetry::resolve_program;

#[test]
fn empty_service_name_uses_the_codegen_program() {
    std::env::set_var("OTEL_SERVICE_NAME", "");
    assert_eq!(resolve_program("from_codegen"), "from_codegen");
}
