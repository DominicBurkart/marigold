use crate::diagnostics::ByteRange;
use crate::parser::Rule;
use pest::iterators::{Pair, Pairs};

#[derive(Debug, Default)]
pub(crate) struct SpanIndex {
    pub enum_range_inputs: Vec<(String, ByteRange)>,
    pub bounded_field_types: Vec<(String, ByteRange)>,
    pub bound_type_refs: Vec<(String, ByteRange)>,
}

fn range_of(pair: &Pair<Rule>) -> ByteRange {
    let span = pair.as_span();
    ByteRange {
        start: span.start(),
        end: span.end(),
    }
}

impl SpanIndex {
    pub fn build(pairs: Pairs<Rule>) -> Self {
        let mut index = Self::default();
        for pair in pairs.flatten() {
            match pair.as_rule() {
                Rule::range_input => {
                    let mut inner = pair.into_inner();
                    if let (Some(name), None) = (inner.next(), inner.next()) {
                        if name.as_rule() == Rule::free_text_identifier {
                            index
                                .enum_range_inputs
                                .push((name.as_str().to_string(), range_of(&name)));
                        }
                    }
                }
                Rule::struct_decl => index.add_struct(pair),
                Rule::bound_type_ref => {
                    if let Some(name) = pair.into_inner().next() {
                        index
                            .bound_type_refs
                            .push((name.as_str().to_string(), range_of(&name)));
                    }
                }
                _ => {}
            }
        }
        index
    }

    fn add_struct(&mut self, pair: Pair<Rule>) {
        let mut inner = pair.into_inner();
        let Some(struct_name) = inner.next().map(|p| p.as_str().to_string()) else {
            return;
        };
        let fields = inner
            .flatten()
            .filter(|p| p.as_rule() == Rule::struct_field);
        for field in fields {
            let mut parts = field.into_inner();
            let (Some(name), Some(ty)) = (parts.next(), parts.next()) else {
                continue;
            };
            let bounded = ty.clone().into_inner().flatten().find(|p| {
                matches!(
                    p.as_rule(),
                    Rule::bounded_int_type | Rule::bounded_uint_type
                )
            });
            if let Some(bounded) = bounded {
                self.bounded_field_types.push((
                    format!("{struct_name}.{}", name.as_str()),
                    range_of(&bounded),
                ));
            }
        }
    }
}
