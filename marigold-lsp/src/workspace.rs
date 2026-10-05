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

use crate::nav::OutlineSymbol;
use lsp_types::Uri;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::Arc;
use std::time::SystemTime;

pub const MAX_FILES: usize = 1000;
pub const MAX_FILE_BYTES: u64 = 10 * 1024 * 1024;
pub const MAX_TOTAL_BYTES: u64 = 64 * 1024 * 1024;
const MAX_ENTRIES: usize = 100_000;
const SKIPPED_DIRS: &[&str] = &["target", "node_modules", ".git"];

/// Whether `root` may be scanned: an absolute path that is not a filesystem or drive root.
///
/// ```
/// use marigold_lsp::workspace::is_usable_root;
/// assert!(!is_usable_root(std::path::Path::new("/")));
/// assert!(!is_usable_root(std::path::Path::new("relative/dir")));
/// ```
pub fn is_usable_root(root: &Path) -> bool {
    root.is_absolute() && root.parent().is_some()
}

/// A `*.marigold` file found by [`discover`], with the metadata that keys the symbol cache.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Discovered {
    pub path: PathBuf,
    pub len: u64,
    pub modified: Option<SystemTime>,
}

/// Finds up to [`MAX_FILES`] `*.marigold` files under `roots` whose sizes sum to at most
/// [`MAX_TOTAL_BYTES`], skipping unusable roots, symlinks, `target/`, `node_modules/` and
/// files over [`MAX_FILE_BYTES`].
///
/// ```
/// assert!(marigold_lsp::workspace::discover(&[std::path::PathBuf::from("/definitely/not/here")]).is_empty());
/// assert!(marigold_lsp::workspace::discover(&[std::path::PathBuf::from("/")]).is_empty());
/// ```
pub fn discover(roots: &[PathBuf]) -> Vec<Discovered> {
    discover_with(roots, MAX_FILES, MAX_TOTAL_BYTES)
}

fn discover_with(roots: &[PathBuf], max_files: usize, max_total: u64) -> Vec<Discovered> {
    let mut files = Vec::new();
    let mut seen = HashSet::new();
    let mut visited = 0usize;
    let mut total = 0u64;
    let mut stack: Vec<PathBuf> = roots
        .iter()
        .filter(|r| is_usable_root(r))
        .rev()
        .cloned()
        .collect();
    while let Some(dir) = stack.pop() {
        let Ok(read) = std::fs::read_dir(&dir) else {
            continue;
        };
        let mut entries: Vec<_> = read.filter_map(Result::ok).collect();
        entries.sort_by_key(|e| e.file_name());
        let mut subdirs = Vec::new();
        for entry in entries {
            visited += 1;
            if visited > MAX_ENTRIES || files.len() >= max_files {
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
            } else if kind.is_file() && path.extension().is_some_and(|e| e == "marigold") {
                let Ok(meta) = entry.metadata() else {
                    continue;
                };
                if meta.len() > MAX_FILE_BYTES || !seen.insert(path.clone()) {
                    continue;
                }
                if total + meta.len() > max_total {
                    return files;
                }
                total += meta.len();
                files.push(Discovered {
                    path,
                    len: meta.len(),
                    modified: meta.modified().ok(),
                });
            }
        }
        stack.extend(subdirs.into_iter().rev());
    }
    files
}

/// Reads the files [`discover`] finds under `roots`.
///
/// ```
/// assert!(marigold_lsp::workspace::scan(&[std::path::PathBuf::from("/definitely/not/here")]).is_empty());
/// ```
pub fn scan(roots: &[PathBuf]) -> Vec<(PathBuf, String)> {
    discover(roots)
        .into_iter()
        .filter_map(|f| std::fs::read_to_string(&f.path).ok().map(|t| (f.path, t)))
        .collect()
}

#[derive(Debug)]
struct CacheEntry {
    len: u64,
    modified: Option<SystemTime>,
    symbols: Arc<Vec<OutlineSymbol>>,
}

/// Declaration names of workspace files, cached by path, size and modification time.
///
/// ```
/// let mut index = marigold_lsp::workspace::SymbolIndex::default();
/// assert!(index.symbols(&[std::path::PathBuf::from("/definitely/not/here")]).is_empty());
/// assert_eq!(index.parse_count(), 0);
/// ```
#[derive(Debug, Default)]
pub struct SymbolIndex {
    entries: HashMap<PathBuf, CacheEntry>,
    parses: usize,
}

impl SymbolIndex {
    pub fn parse_count(&self) -> usize {
        self.parses
    }

    pub fn symbols(&mut self, roots: &[PathBuf]) -> Vec<(PathBuf, Arc<Vec<OutlineSymbol>>)> {
        self.symbols_with(roots, MAX_FILES, MAX_TOTAL_BYTES)
    }

    fn symbols_with(
        &mut self,
        roots: &[PathBuf],
        max_files: usize,
        max_total: u64,
    ) -> Vec<(PathBuf, Arc<Vec<OutlineSymbol>>)> {
        let found = discover_with(roots, max_files, max_total);
        let live: HashSet<&PathBuf> = found.iter().map(|f| &f.path).collect();
        self.entries.retain(|p, _| live.contains(p));
        let mut out = Vec::with_capacity(found.len());
        for file in &found {
            let fresh = self
                .entries
                .get(&file.path)
                .is_some_and(|e| e.len == file.len && e.modified == file.modified);
            if !fresh {
                let Ok(text) = std::fs::read_to_string(&file.path) else {
                    self.entries.remove(&file.path);
                    continue;
                };
                self.parses += 1;
                let symbols = Arc::new(crate::nav::declaration_names(&text));
                self.entries.insert(
                    file.path.clone(),
                    CacheEntry {
                        len: file.len,
                        modified: file.modified,
                        symbols,
                    },
                );
            }
            out.push((
                file.path.clone(),
                Arc::clone(&self.entries[&file.path].symbols),
            ));
        }
        out
    }
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

    fn scratch(tag: &str) -> PathBuf {
        let dir = std::env::temp_dir().join(format!("marigold-wsu-{tag}-{}", std::process::id()));
        let _ = std::fs::remove_dir_all(&dir);
        std::fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn roots_must_be_absolute_and_not_filesystem_roots() {
        assert!(!is_usable_root(Path::new("/")));
        assert!(!is_usable_root(Path::new("")));
        assert!(!is_usable_root(Path::new("rel/dir")));
        assert!(!is_usable_root(Path::new(".")));
        assert!(is_usable_root(&std::env::temp_dir()));
        if cfg!(windows) {
            assert!(!is_usable_root(Path::new("C:\\")));
            assert!(!is_usable_root(Path::new("C:")));
        }
    }

    #[test]
    fn discover_ignores_unusable_roots() {
        let dir = scratch("roots");
        std::fs::write(dir.join("a.marigold"), "x = range(0, 1)").unwrap();
        assert!(discover(&[PathBuf::from("/")]).is_empty());
        let rel = PathBuf::from(dir.file_name().unwrap());
        assert!(discover(&[rel]).is_empty());
        assert_eq!(discover(std::slice::from_ref(&dir)).len(), 1);
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn total_byte_budget_stops_discovery() {
        let dir = scratch("budget");
        for name in ["a", "b", "c", "d"] {
            std::fs::write(dir.join(format!("{name}.marigold")), "x".repeat(100)).unwrap();
        }
        assert_eq!(
            discover_with(std::slice::from_ref(&dir), 1000, 250).len(),
            2
        );
        assert_eq!(
            discover_with(std::slice::from_ref(&dir), 1000, 400).len(),
            4
        );
        assert_eq!(discover_with(std::slice::from_ref(&dir), 1000, 99).len(), 0);
        assert_eq!(
            discover_with(std::slice::from_ref(&dir), 3, 10_000).len(),
            3
        );
        std::fs::remove_dir_all(dir).unwrap();
    }

    #[test]
    fn default_total_budget_is_64_mib() {
        assert_eq!(MAX_TOTAL_BYTES, 64 * 1024 * 1024);
    }

    #[test]
    fn index_reuses_unchanged_files_and_reparses_changed_ones() {
        let dir = scratch("cache");
        let file = dir.join("a.marigold");
        std::fs::write(&file, "x = range(0, 1)").unwrap();
        let mut index = SymbolIndex::default();
        let roots = [dir.clone()];
        let first = index.symbols(&roots);
        assert_eq!(first[0].1[0].name, "x");
        assert_eq!(index.parse_count(), 1);
        index.symbols(&roots);
        index.symbols(&roots);
        assert_eq!(index.parse_count(), 1);
        std::fs::write(&file, "y = range(0, 1)").unwrap();
        let handle = std::fs::OpenOptions::new().write(true).open(&file).unwrap();
        handle
            .set_modified(SystemTime::now() + std::time::Duration::from_secs(5))
            .unwrap();
        let changed = index.symbols(&roots);
        assert_eq!(changed[0].1[0].name, "y");
        assert_eq!(index.parse_count(), 2);
        std::fs::remove_file(&file).unwrap();
        assert!(index.symbols(&roots).is_empty());
        std::fs::remove_dir_all(dir).unwrap();
    }
}
