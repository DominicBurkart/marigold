//! Bounded discovery of `*.marigold` files and symbol-name matching.
//!
//! ```
//! use marigold_lsp::workspace::match_score;
//!
//! assert_eq!(match_score("tot", "total"), Some(1));
//! assert_eq!(match_score("TOT", "my_total"), Some(2));
//! assert_eq!(match_score("tl", "total"), Some(3));
//! assert_eq!(match_score("zz", "total"), None);
//! ```

use lsp_types::Uri;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::str::FromStr;

pub const MAX_FILES: usize = 1000;
pub const MAX_FILE_BYTES: u64 = 10 * 1024 * 1024;
const MAX_ENTRIES: usize = 100_000;
const SKIPPED_DIRS: &[&str] = &["target", "node_modules", ".git"];

/// Reads up to [`MAX_FILES`] `*.marigold` files under `roots`, skipping symlinks,
/// `target/`, `node_modules/` and files over [`MAX_FILE_BYTES`].
///
/// ```
/// assert!(marigold_lsp::workspace::scan(&[std::path::PathBuf::from("/definitely/not/here")]).is_empty());
/// ```
pub fn scan(roots: &[PathBuf]) -> Vec<(PathBuf, String)> {
    let mut files = Vec::new();
    let mut seen = HashSet::new();
    let mut visited = 0usize;
    let mut stack: Vec<PathBuf> = roots.iter().rev().cloned().collect();
    while let Some(dir) = stack.pop() {
        let Ok(read) = std::fs::read_dir(&dir) else {
            continue;
        };
        let mut entries: Vec<_> = read.filter_map(Result::ok).collect();
        entries.sort_by_key(|e| e.file_name());
        let mut subdirs = Vec::new();
        for entry in entries {
            visited += 1;
            if visited > MAX_ENTRIES || files.len() >= MAX_FILES {
                return files;
            }
            let Ok(kind) = entry.file_type() else {
                continue;
            };
            let path = entry.path();
            if kind.is_symlink() {
                continue;
            }
            if kind.is_dir() {
                let skipped = entry
                    .file_name()
                    .to_str()
                    .is_some_and(|n| SKIPPED_DIRS.contains(&n));
                if !skipped {
                    subdirs.push(path);
                }
            } else if kind.is_file()
                && path.extension().is_some_and(|e| e == "marigold")
                && entry.metadata().is_ok_and(|m| m.len() <= MAX_FILE_BYTES)
                && seen.insert(path.clone())
            {
                if let Ok(text) = std::fs::read_to_string(&path) {
                    files.push((path, text));
                }
            }
        }
        stack.extend(subdirs.into_iter().rev());
    }
    files
}

fn percent_decode(s: &str) -> Option<String> {
    let bytes = s.as_bytes();
    let mut out = Vec::with_capacity(bytes.len());
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' {
            let hex = s.get(i + 1..i + 3)?;
            out.push(u8::from_str_radix(hex, 16).ok()?);
            i += 3;
        } else {
            out.push(bytes[i]);
            i += 1;
        }
    }
    String::from_utf8(out).ok()
}

fn has_drive(path: &str) -> bool {
    let b = path.as_bytes();
    b.len() >= 2 && b[0].is_ascii_alphabetic() && b[1] == b':' && (b.len() == 2 || b[2] == b'/')
}

/// Percent-decodes the path component of a `file:` URI. With `windows`, a
/// leading `/C:` drive prefix becomes `C:` with an upper-case drive letter.
///
/// ```
/// use marigold_lsp::workspace::decode_file_path;
/// assert_eq!(decode_file_path("/tmp/a%20b", false).unwrap(), "/tmp/a b");
/// assert_eq!(decode_file_path("/c%3A/x", true).unwrap(), "C:/x");
/// ```
pub fn decode_file_path(path: &str, windows: bool) -> Option<String> {
    let decoded = percent_decode(path)?;
    if windows {
        if let Some(rest) = decoded.strip_prefix('/') {
            if has_drive(rest) {
                let mut chars = rest.chars();
                let drive = chars.next()?.to_ascii_uppercase();
                return Some(format!("{drive}{}", chars.as_str()));
            }
        }
    }
    Some(decoded)
}

/// Builds a `file://` URI from a path string. With `windows`, backslashes become
/// slashes and a drive path `C:\x` becomes `file:///C:/x`.
///
/// ```
/// use marigold_lsp::workspace::encode_file_uri;
/// assert_eq!(encode_file_uri("/tmp/a b", false), "file:///tmp/a%20b");
/// assert_eq!(encode_file_uri("C:\\x\\y", true), "file:///C:/x/y");
/// ```
pub fn encode_file_uri(path: &str, windows: bool) -> String {
    let mut p = if windows {
        path.replace('\\', "/")
    } else {
        path.to_string()
    };
    let drive = windows && has_drive(&p);
    if drive {
        p = format!("/{}{}", p[..1].to_ascii_uppercase(), &p[1..]);
    }
    let unc = windows && p.starts_with("//");
    let mut out = String::from(if unc { "file:" } else { "file://" });
    for (i, &b) in p.as_bytes().iter().enumerate() {
        let keep_colon = drive && i == 2 && b == b':';
        if b.is_ascii_alphanumeric() || b"/-._~".contains(&b) || keep_colon {
            out.push(b as char);
        } else {
            out.push_str(&format!("%{b:02X}"));
        }
    }
    out
}

/// Converts a `file://` URI to a filesystem path, handling percent escapes on
/// every platform and `file:///C:/x` drive paths on Windows.
///
/// ```
/// use std::str::FromStr;
/// let uri = lsp_types::Uri::from_str("file:///tmp/a%20b").unwrap();
/// if !cfg!(windows) {
///     assert_eq!(marigold_lsp::workspace::uri_to_path(&uri).unwrap(), std::path::PathBuf::from("/tmp/a b"));
/// }
/// let web = lsp_types::Uri::from_str("https://example.com/a").unwrap();
/// assert!(marigold_lsp::workspace::uri_to_path(&web).is_none());
/// ```
pub fn uri_to_path(uri: &Uri) -> Option<PathBuf> {
    if !uri.scheme()?.as_str().eq_ignore_ascii_case("file") {
        return None;
    }
    let path = decode_file_path(uri.path().as_str(), cfg!(windows))?;
    let host = uri
        .authority()
        .map(|a| a.host().to_string())
        .filter(|h| !h.is_empty() && !h.eq_ignore_ascii_case("localhost"));
    match host {
        None => Some(PathBuf::from(path)),
        Some(host) if cfg!(windows) => Some(PathBuf::from(format!("//{host}{path}"))),
        Some(_) => None,
    }
}

/// Converts a path to a `file://` URI, or `None` if it is not valid UTF-8.
///
/// ```
/// if !cfg!(windows) {
///     let uri = marigold_lsp::workspace::path_to_uri(std::path::Path::new("/tmp/a b.marigold")).unwrap();
///     assert_eq!(uri.as_str(), "file:///tmp/a%20b.marigold");
/// }
/// ```
pub fn path_to_uri(path: &Path) -> Option<Uri> {
    Uri::from_str(&encode_file_uri(path.to_str()?, cfg!(windows))).ok()
}

/// Ranks how well `query` matches `name`; lower is better, `None` is no match.
/// 0 exact, 1 prefix, 2 substring, 3 subsequence, all case-insensitive.
pub fn match_score(query: &str, name: &str) -> Option<u8> {
    let q = query.to_lowercase();
    let n = name.to_lowercase();
    if q.is_empty() || n == q {
        return Some(0);
    }
    if n.starts_with(&q) {
        return Some(1);
    }
    if n.contains(&q) {
        return Some(2);
    }
    let mut rest = n.chars();
    q.chars().all(|c| rest.any(|x| x == c)).then_some(3)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn decodes_posix_paths_with_percent_escapes() {
        assert_eq!(decode_file_path("/tmp/a%20b", false).unwrap(), "/tmp/a b");
        assert_eq!(decode_file_path("/t/%C3%BC", false).unwrap(), "/t/\u{fc}");
        assert!(decode_file_path("/t/%zz", false).is_none());
        assert!(decode_file_path("/t/%FF", false).is_none());
        assert!(decode_file_path("/t/%4", false).is_none());
    }

    #[test]
    fn decodes_windows_drive_paths() {
        assert_eq!(decode_file_path("/C:/x/y", true).unwrap(), "C:/x/y");
        assert_eq!(
            decode_file_path("/c%3A/My%20Dir/f", true).unwrap(),
            "C:/My Dir/f"
        );
        assert_eq!(decode_file_path("/c:/x", true).unwrap(), "C:/x");
    }

    #[test]
    fn windows_mode_leaves_non_drive_paths_alone() {
        assert_eq!(decode_file_path("/tmp/x", true).unwrap(), "/tmp/x");
    }

    #[test]
    fn encodes_posix_paths() {
        assert_eq!(
            encode_file_uri("/tmp/a b\u{fc}.marigold", false),
            "file:///tmp/a%20b%C3%BC.marigold"
        );
    }

    #[test]
    fn encodes_windows_paths() {
        assert_eq!(
            encode_file_uri("C:\\Users\\me\\a b.marigold", true),
            "file:///C:/Users/me/a%20b.marigold"
        );
        assert_eq!(encode_file_uri("c:/x", true), "file:///C:/x");
    }

    #[test]
    fn windows_round_trip() {
        let uri = encode_file_uri("D:\\w s\\f.marigold", true);
        let path = decode_file_path(uri.strip_prefix("file://").unwrap(), true).unwrap();
        assert_eq!(path, "D:/w s/f.marigold");
    }

    #[test]
    fn uri_wrappers_use_host_conventions() {
        use std::str::FromStr;
        let uri = Uri::from_str("file:///tmp/a%20b").unwrap();
        let path = uri_to_path(&uri);
        if cfg!(windows) {
            assert_eq!(path.unwrap(), PathBuf::from("/tmp/a b"));
        } else {
            assert_eq!(path.unwrap(), PathBuf::from("/tmp/a b"));
            assert_eq!(path_to_uri(Path::new("/tmp/a b")).unwrap(), uri);
        }
        let drive = Uri::from_str("file:///C:/x").unwrap();
        if cfg!(windows) {
            assert_eq!(uri_to_path(&drive).unwrap(), PathBuf::from("C:/x"));
        }
        assert!(uri_to_path(&Uri::from_str("https://h/p").unwrap()).is_none());
        assert!(uri_to_path(&Uri::from_str("file://remote/p").unwrap()).is_none() || cfg!(windows));
    }
}
