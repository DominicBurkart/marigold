use std::ops::Range;

fn starts_expression(c: char) -> bool {
    c.is_alphabetic() || c == '_'
}

fn ends_with_declaration_keyword(trimmed: &str) -> bool {
    let word_start = trimmed
        .rfind(|c: char| !(c.is_alphanumeric() || c == '_'))
        .map_or(0, |i| {
            i + trimmed[i..].chars().next().map_or(1, char::len_utf8)
        });
    let preceded_by_dot = trimmed[..word_start].ends_with('.');
    !preceded_by_dot && matches!(&trimmed[word_start..], "fn" | "struct" | "enum")
}

fn continues_previous_line(before: &str) -> bool {
    let trimmed = before.trim_end();
    trimmed.ends_with("->")
        || trimmed.ends_with(['=', '.', ',', '('])
        || ends_with_declaration_keyword(trimmed)
}

pub(crate) fn top_level_chunks(src: &str) -> Vec<Range<usize>> {
    let bytes = src.as_bytes();
    let mut starts = vec![0usize];
    let mut depth = 0usize;
    let mut in_string = false;
    let mut i = 0usize;
    while i < bytes.len() {
        let b = bytes[i];
        if in_string {
            if b == b'\\' && depth > 0 {
                i += 1;
            } else if b == b'"' {
                in_string = false;
            }
            i += 1;
            continue;
        }
        match b {
            b'"' => in_string = true,
            b'(' | b'[' | b'{' => depth += 1,
            b')' | b']' | b'}' => depth = depth.saturating_sub(1),
            b'/' if depth > 0 && bytes.get(i + 1) == Some(&b'/') => {
                while i < bytes.len() && bytes[i] != b'\n' {
                    i += 1;
                }
                continue;
            }
            b'/' if depth > 0 && bytes.get(i + 1) == Some(&b'*') => {
                i += 2;
                while i < bytes.len() && !(bytes[i] == b'*' && bytes.get(i + 1) == Some(&b'/')) {
                    i += 1;
                }
                i += 2;
                continue;
            }
            b'\'' if depth > 0 => {
                if let Some(c) = src[i + 1..].chars().next() {
                    let width = if c == '\\' {
                        src[i + 2..].chars().next().map_or(1, |n| 1 + n.len_utf8())
                    } else {
                        c.len_utf8()
                    };
                    if bytes.get(i + 1 + width) == Some(&b'\'') {
                        i += width + 2;
                        continue;
                    }
                }
            }
            b'\n' if depth == 0 => {
                let rest = &src[i + 1..];
                let next = rest.trim_start();
                if next.chars().next().is_some_and(starts_expression)
                    && !continues_previous_line(&src[..i])
                {
                    let line_start = i
                        + 1
                        + rest[..rest.len() - next.len()]
                            .rfind('\n')
                            .map_or(0, |n| n + 1);
                    if starts.last() != Some(&line_start) {
                        starts.push(line_start);
                    }
                }
            }
            _ => {}
        }
        i += 1;
    }
    let mut chunks: Vec<Range<usize>> = starts.windows(2).map(|w| w[0]..w[1]).collect();
    chunks.push(*starts.last().unwrap_or(&0)..src.len());
    chunks
}

#[cfg(test)]
mod tests {
    use super::*;

    fn pieces(src: &str) -> Vec<&str> {
        top_level_chunks(src).into_iter().map(|r| &src[r]).collect()
    }

    #[test]
    fn splits_at_newlines_before_expression_starts() {
        assert_eq!(
            pieces("a.return\nb.return\nc.return"),
            vec!["a.return\n", "b.return\n", "c.return"]
        );
    }

    #[test]
    fn does_not_split_inside_brackets() {
        assert_eq!(pieces("a(\nb\n).return"), vec!["a(\nb\n).return"]);
    }

    #[test]
    fn does_not_split_before_continuation_lines() {
        assert_eq!(
            pieces("a\n  .map(f)\n  .return"),
            vec!["a\n  .map(f)\n  .return"]
        );
        assert_eq!(pieces("x =\nrange(0, 1)"), vec!["x =\nrange(0, 1)"]);
    }

    #[test]
    fn does_not_split_after_a_lone_declaration_keyword() {
        assert_eq!(pieces("fn\nf()"), vec!["fn\nf()"]);
        assert_eq!(pieces("struct\nRow"), vec!["struct\nRow"]);
        assert_eq!(pieces("enum\nE"), vec!["enum\nE"]);
        assert_eq!(pieces("a.fn\nb"), vec!["a.fn\n", "b"]);
        assert_eq!(pieces("fnx\nb"), vec!["fnx\n", "b"]);
    }

    #[test]
    fn does_not_split_inside_block_comments_in_bodies() {
        assert_eq!(
            pieces("fn f() { /* }\nx */ }\nb"),
            vec!["fn f() { /* }\nx */ }\n", "b"]
        );
    }

    #[test]
    fn does_not_split_inside_strings() {
        assert_eq!(pieces("a(\"x\ny\").return"), vec!["a(\"x\ny\").return"]);
        assert_eq!(pieces("a(\"x\ny\")\nb"), vec!["a(\"x\ny\")\n", "b"]);
    }

    #[test]
    fn blank_lines_belong_to_the_following_chunk_start_line() {
        assert_eq!(pieces("a\n\n\nb"), vec!["a\n\n\n", "b"]);
    }

    #[test]
    fn chunks_cover_the_whole_input_on_char_boundaries() {
        let src = "\u{fc}\n\u{e9}(\n\u{e9})\n_x\n";
        let chunks = top_level_chunks(src);
        assert_eq!(chunks.first().unwrap().start, 0);
        assert_eq!(chunks.last().unwrap().end, src.len());
        for pair in chunks.windows(2) {
            assert_eq!(pair[0].end, pair[1].start);
        }
        for c in &chunks {
            assert!(src.is_char_boundary(c.start) && src.is_char_boundary(c.end));
        }
    }
}
