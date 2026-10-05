#![cfg(feature = "otel")]

use marigold_grammar::instrument::{CodegenOptions, SiteRegistry};
use marigold_grammar::node_ids::NodeKind;
use marigold_grammar::{marigold_node_ids, marigold_parse_instrumented};
use std::collections::BTreeSet;

const NAMED: &str = "x = range(0, 5)\nx.return";
const ANON: &str = "range(0, 3).return";

fn embedded(src: &str, options: &CodegenOptions) -> BTreeSet<String> {
    let code = marigold_parse_instrumented(src, options).unwrap();
    code.match_indices("NodeMeta::new(\"mg1-")
        .map(|(at, m)| {
            let start = at + m.len() - 4;
            code[start..start + 20].to_string()
        })
        .collect()
}

#[test]
fn first_site_keeps_the_ids_the_language_server_computes() {
    let registry = SiteRegistry::new();
    let options = registry.options(NAMED, "demo", "bytes(10..20)");
    assert_eq!(options.discriminator(), None);
    assert_eq!(options.program(), "demo");
    let wanted: BTreeSet<String> = marigold_node_ids(NAMED, "demo")
        .into_iter()
        .filter(|n| n.kind != NodeKind::VariableDeclaration)
        .map(|n| n.id.to_string())
        .collect();
    assert_eq!(embedded(NAMED, &options), wanted);
}

#[test]
fn same_variable_name_at_another_site_gets_distinct_ids() {
    let registry = SiteRegistry::new();
    let first = registry.options(NAMED, "demo", "bytes(10..20)");
    let second = registry.options("x = range(7, 9)\nx.return", "demo", "bytes(50..60)");
    assert_eq!(first.discriminator(), None);
    assert!(second.discriminator().is_some());
    let a = embedded(NAMED, &first);
    let b = embedded("x = range(7, 9)\nx.return", &second);
    assert!(a.is_disjoint(&b));
}

#[test]
fn identical_unnamed_programs_at_two_sites_get_distinct_ids() {
    let registry = SiteRegistry::new();
    let first = registry.options(ANON, "demo", "bytes(1..2)");
    let second = registry.options(ANON, "demo", "bytes(30..31)");
    assert!(embedded(ANON, &first).is_disjoint(&embedded(ANON, &second)));
}

#[test]
fn discriminated_ids_are_computed_from_the_id_namespace() {
    let registry = SiteRegistry::new();
    registry.options(ANON, "demo", "bytes(1..2)");
    let second = registry.options(ANON, "demo", "bytes(30..31)");
    let namespace = second.id_namespace();
    assert_ne!(namespace, "demo");
    assert!(namespace.starts_with("demo"));
    let wanted: BTreeSet<String> = marigold_node_ids(ANON, &namespace)
        .into_iter()
        .map(|n| n.id.to_string())
        .collect();
    assert_eq!(embedded(ANON, &second), wanted);
}

#[test]
fn re_expanding_a_site_is_idempotent() {
    let registry = SiteRegistry::new();
    registry.options(ANON, "demo", "bytes(1..2)");
    let second = registry.options(ANON, "demo", "bytes(30..31)");
    let again = registry.options(ANON, "demo", "bytes(30..31)");
    assert_eq!(second, again);
    let first_again = registry.options(ANON, "demo", "bytes(1..2)");
    assert_eq!(first_again.discriminator(), None);
}

#[test]
fn editing_the_program_at_a_site_forgets_its_old_ids() {
    let registry = SiteRegistry::new();
    registry.options(NAMED, "demo", "bytes(1..2)");
    let edited = registry.options("x = range(0, 6)\nx.return", "demo", "bytes(1..2)");
    assert_eq!(edited.discriminator(), None);
}

#[test]
fn programs_without_shared_ids_are_never_discriminated() {
    let registry = SiteRegistry::new();
    let a = registry.options("a = range(0, 5)\na.return", "demo", "bytes(1..2)");
    let b = registry.options("b = range(0, 5)\nb.return", "demo", "bytes(3..4)");
    let c = registry.options("range(0, 9).return", "demo", "bytes(5..6)");
    assert_eq!(a.discriminator(), None);
    assert_eq!(b.discriminator(), None);
    assert_eq!(c.discriminator(), None);
}

#[test]
fn different_programs_do_not_collide() {
    let registry = SiteRegistry::new();
    let a = registry.options(ANON, "one", "bytes(1..2)");
    let b = registry.options(ANON, "two", "bytes(3..4)");
    assert_eq!(a.discriminator(), None);
    assert_eq!(b.discriminator(), None);
}

#[test]
fn many_sites_all_get_unique_ids() {
    let registry = SiteRegistry::new();
    let mut seen = BTreeSet::new();
    for i in 0..20 {
        let options = registry.options(ANON, "demo", &format!("bytes({i}..{})", i + 1));
        for id in embedded(ANON, &options) {
            assert!(seen.insert(id));
        }
    }
}

#[test]
fn registry_is_shareable_between_threads() {
    let registry = std::sync::Arc::new(SiteRegistry::new());
    let handles: Vec<_> = (0..8)
        .map(|i| {
            let registry = registry.clone();
            std::thread::spawn(move || {
                embedded(
                    ANON,
                    &registry.options(ANON, "demo", &format!("bytes({i}..{})", i + 100)),
                )
            })
        })
        .collect();
    let mut seen = BTreeSet::new();
    for handle in handles {
        for id in handle.join().unwrap() {
            assert!(seen.insert(id));
        }
    }
}

#[test]
fn explicit_discriminator_changes_only_the_ids() {
    let plain = CodegenOptions::new("demo");
    let tagged = CodegenOptions::new("demo").with_discriminator("abc");
    assert_eq!(tagged.discriminator(), Some("abc"));
    assert_eq!(plain.id_namespace(), "demo");
    assert!(embedded(ANON, &plain).is_disjoint(&embedded(ANON, &tagged)));
    let code = marigold_parse_instrumented(ANON, &tagged).unwrap();
    assert!(code.contains("\"demo\", file!()"));
}
