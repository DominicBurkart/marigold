#[cfg(kani)]
mod proofs {
    use marigold_grammar::complexity::ComplexityClass;

    /// Verify ComplexityClass ordering is reflexive
    #[kani::proof]
    fn complexity_class_ordering_reflexive() {
        // ComplexityClass variants that can be constructed without parameters
        let classes = [
            ComplexityClass::O1,
            ComplexityClass::OLogN,
            ComplexityClass::ON,
            ComplexityClass::ONLogN,
            ComplexityClass::OFactorial,
            ComplexityClass::Unknown,
        ];
        for &ref c in &classes {
            assert!(c <= c, "ComplexityClass ordering must be reflexive");
        }
    }
}
