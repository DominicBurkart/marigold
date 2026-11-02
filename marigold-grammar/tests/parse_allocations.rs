use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering::Relaxed};

struct Counting;
static ALLOCATIONS: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for Counting {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        System.alloc(layout)
    }
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout)
    }
    unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
        ALLOCATIONS.fetch_add(1, Relaxed);
        System.realloc(ptr, layout, new_size)
    }
}

#[global_allocator]
static GLOBAL: Counting = Counting;

const PROGRAMS: &[&str] = &[
    "range(0, 1).return",
    "range(0, 5).map(f).return",
    "x = range(0, 5)\nx.return",
    "struct S { a: u8 }\nrange(0, 1).return",
    "enum E { A, B }\nrange(E).return",
    "fn f(x: i32) -> i32 { x }\nrange(0, 3).map(f).return",
    "range(0, 3).write_file(\"o.csv\", csv)",
];

fn allocations_of(f: impl Fn()) -> usize {
    f();
    let before = ALLOCATIONS.load(Relaxed);
    f();
    ALLOCATIONS.load(Relaxed) - before
}

#[test]
fn marigold_parse_allocates_clearly_less_than_marigold_generate() {
    let mut report = Vec::new();
    for program in PROGRAMS {
        let parse = allocations_of(|| {
            std::hint::black_box(marigold_grammar::marigold_parse(program).unwrap());
        });
        let generate = allocations_of(|| {
            std::hint::black_box(marigold_grammar::marigold_generate(program).unwrap());
        });
        report.push(format!("{program:?}: parse {parse}, generate {generate}"));
        assert!(
            parse + 6 <= generate,
            "marigold_parse is within 6 allocations of marigold_generate, so it pays for the source map:\n{}",
            report.join("\n")
        );
    }
}
