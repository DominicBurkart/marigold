#![cfg(feature = "otel")]

use marigold_impl::telemetry::resolve_program;

#[test]
fn unset_service_name_uses_the_codegen_program() {
    std::env::remove_var("OTEL_SERVICE_NAME");
    assert_eq!(resolve_program("from_codegen"), "from_codegen");
}
