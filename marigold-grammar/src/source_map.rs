//! Red-phase stub: signatures only (see tests/source_map.rs).
use crate::diagnostics::ByteRange;
use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize)]
#[serde(rename_all = "snake_case")]
#[non_exhaustive]
pub enum SegmentKind {
    Struct,
    Enum,
    Function,
    StreamVariable,
    StreamVariableRunner,
    ReturningStream,
    NonReturningStream,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct Segment {
    pub generated: ByteRange,
    pub source: ByteRange,
    pub kind: SegmentKind,
    pub expr_index: usize,
    pub name: Option<String>,
}

impl Segment {
    pub fn new(
        _generated: ByteRange,
        _source: ByteRange,
        _kind: SegmentKind,
        _expr_index: usize,
        _name: Option<String>,
    ) -> Segment {
        unimplemented!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct SourceMap {
    segments: Vec<Segment>,
}

impl SourceMap {
    pub fn new(_segments: Vec<Segment>) -> SourceMap {
        unimplemented!()
    }

    pub fn segments(&self) -> &[Segment] {
        unimplemented!()
    }

    pub fn source_for_generated(&self, _offset: usize) -> Option<&Segment> {
        unimplemented!()
    }

    pub fn generated_for_source(&self, _range: ByteRange) -> Vec<&Segment> {
        unimplemented!()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[non_exhaustive]
pub struct GeneratedProgram {
    pub rust: String,
    pub map: SourceMap,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn r(start: usize, end: usize) -> ByteRange {
        ByteRange { start, end }
    }

    fn seg(gen: (usize, usize), src: (usize, usize), kind: SegmentKind, expr: usize) -> Segment {
        Segment::new(r(gen.0, gen.1), r(src.0, src.1), kind, expr, None)
    }

    /// Flat map whose generated order differs from source order, with two segments sharing one source range.
    fn flat() -> SourceMap {
        SourceMap::new(vec![
            seg((10, 20), (50, 60), SegmentKind::Function, 2),
            seg((22, 30), (0, 10), SegmentKind::StreamVariable, 0),
            seg((32, 40), (20, 30), SegmentKind::ReturningStream, 1),
            seg((42, 50), (0, 10), SegmentKind::StreamVariableRunner, 0),
        ])
    }

    fn ids(v: Vec<&Segment>) -> Vec<(usize, SegmentKind)> {
        v.into_iter().map(|s| (s.expr_index, s.kind)).collect()
    }

    #[test]
    fn new_sorts_segments_into_generated_order_outer_first() {
        let m = SourceMap::new(vec![
            seg((5, 6), (0, 1), SegmentKind::ReturningStream, 3),
            seg((0, 10), (0, 10), SegmentKind::Function, 2),
            seg((0, 4), (2, 3), SegmentKind::Struct, 1),
        ]);
        let order: Vec<_> = m.segments().iter().map(|s| s.expr_index).collect();
        assert_eq!(order, vec![2, 1, 3]);
    }

    #[test]
    fn source_for_generated_flat_boundaries() {
        let m = flat();
        let at = |o| m.source_for_generated(o).map(|s| s.expr_index);
        assert_eq!(at(0), None);
        assert_eq!(at(9), None);
        assert_eq!(at(10), Some(2));
        assert_eq!(at(19), Some(2));
        assert_eq!(at(20), None);
        assert_eq!(at(21), None);
        assert_eq!(at(22), Some(0));
        assert_eq!(at(42), Some(0));
        assert_eq!(at(49), Some(0));
        assert_eq!(at(50), None);
        assert_eq!(at(usize::MAX), None);
    }

    #[test]
    fn source_for_generated_returns_the_innermost_of_nested_segments() {
        let m = SourceMap::new(vec![
            seg((0, 100), (0, 50), SegmentKind::ReturningStream, 0),
            seg((10, 20), (5, 10), SegmentKind::StreamVariable, 0),
            seg((15, 18), (6, 7), SegmentKind::StreamVariableRunner, 0),
            seg((30, 40), (20, 30), SegmentKind::Struct, 0),
        ]);
        let kind = |o| m.source_for_generated(o).map(|s| s.kind);
        assert_eq!(kind(5), Some(SegmentKind::ReturningStream));
        assert_eq!(kind(10), Some(SegmentKind::StreamVariable));
        assert_eq!(kind(14), Some(SegmentKind::StreamVariable));
        assert_eq!(kind(15), Some(SegmentKind::StreamVariableRunner));
        assert_eq!(kind(17), Some(SegmentKind::StreamVariableRunner));
        assert_eq!(kind(18), Some(SegmentKind::StreamVariable));
        assert_eq!(kind(20), Some(SegmentKind::ReturningStream));
        assert_eq!(kind(35), Some(SegmentKind::Struct));
        assert_eq!(kind(99), Some(SegmentKind::ReturningStream));
        assert_eq!(kind(100), None);
    }

    #[test]
    fn generated_for_source_returns_overlaps_in_generated_order() {
        let m = flat();
        let q = |a, b| ids(m.generated_for_source(r(a, b)));
        assert_eq!(
            q(0, 10),
            vec![
                (0, SegmentKind::StreamVariable),
                (0, SegmentKind::StreamVariableRunner)
            ]
        );
        assert_eq!(q(0, 100).len(), 4);
        assert_eq!(
            q(0, 100)[0].0,
            2,
            "generated order puts the fn (source 50..60) first"
        );
        assert_eq!(
            q(9, 21),
            vec![
                (0, SegmentKind::StreamVariable),
                (1, SegmentKind::ReturningStream),
                (0, SegmentKind::StreamVariableRunner),
            ]
        );
        assert!(q(10, 20).is_empty(), "gap between source ranges");
        assert!(q(60, 70).is_empty(), "half-open: 60 is outside 50..60");
        assert_eq!(q(59, 60), vec![(2, SegmentKind::Function)]);
        assert!(q(10, 10).is_empty());
        assert_eq!(q(0, 0).len(), 2);
        assert_eq!(q(9, 9).len(), 2);
        assert!(q(7, 3).is_empty());
        assert!(q(usize::MAX - 1, usize::MAX).is_empty());
    }

    #[test]
    fn generated_for_source_handles_nested_source_ranges() {
        let m = SourceMap::new(vec![
            seg((0, 100), (0, 50), SegmentKind::ReturningStream, 0),
            seg((10, 20), (5, 10), SegmentKind::StreamVariable, 0),
            seg((30, 40), (40, 45), SegmentKind::Struct, 0),
        ]);
        let q = |a, b| m.generated_for_source(r(a, b)).len();
        assert_eq!(q(7, 8), 2);
        assert_eq!(q(20, 30), 1);
        assert_eq!(q(42, 43), 2);
        assert_eq!(q(50, 60), 0);
    }

    #[test]
    fn empty_map_answers_none_and_empty() {
        let m = SourceMap::new(Vec::new());
        assert!(m.segments().is_empty());
        assert!(m.source_for_generated(0).is_none());
        assert!(m.generated_for_source(r(0, 100)).is_empty());
    }

    #[test]
    fn lookups_scale_to_many_segments() {
        let n = 100_000;
        let segments: Vec<Segment> = (0..n)
            .map(|i| {
                let g = i * 10;
                let s = (n - 1 - i) * 10; // reversed: generated order != source order
                seg((g, g + 8), (s, s + 8), SegmentKind::ReturningStream, i)
            })
            .collect();
        let m = SourceMap::new(segments);
        let t = std::time::Instant::now();
        let mut hits = 0;
        for i in 0..n {
            hits += m.source_for_generated(i * 10 + 3).is_some() as usize;
            hits += m.source_for_generated(i * 10 + 9).is_some() as usize;
            hits += m.generated_for_source(r(i * 10, i * 10 + 8)).len();
        }
        assert_eq!(hits, n + n);
        // O(n log n): milliseconds. O(n^2) would need ~2*10^10 comparisons.
        assert!(t.elapsed().as_secs() < 5, "{:?}", t.elapsed());
    }
}
