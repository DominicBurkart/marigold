mod common;

use common::{uri, Client};
use lsp_types::request::WorkspaceSymbolRequest;
use lsp_types::{
    InitializeParams, SymbolInformation, SymbolKind, WorkspaceFolder, WorkspaceSymbolParams,
    WorkspaceSymbolResponse,
};
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use std::sync::atomic::{AtomicUsize, Ordering};

static COUNTER: AtomicUsize = AtomicUsize::new(0);

fn scratch() -> PathBuf {
    let dir = std::env::temp_dir().join(format!(
        "marigold-ws-{}-{}",
        std::process::id(),
        COUNTER.fetch_add(1, Ordering::SeqCst)
    ));
    fs::create_dir_all(&dir).unwrap();
    dir
}

fn folder_uri(path: &Path) -> lsp_types::Uri {
    marigold_lsp::workspace::path_to_uri(path).unwrap()
}

fn init_with_folder(path: &Path) -> InitializeParams {
    InitializeParams {
        workspace_folders: Some(vec![WorkspaceFolder {
            uri: folder_uri(path),
            name: "ws".into(),
        }]),
        ..Default::default()
    }
}

fn query(client: &mut Client, q: &str) -> Vec<SymbolInformation> {
    let value = client.request::<WorkspaceSymbolRequest>(WorkspaceSymbolParams {
        query: q.to_string(),
        work_done_progress_params: Default::default(),
        partial_result_params: Default::default(),
    });
    match serde_json::from_value(value).unwrap() {
        WorkspaceSymbolResponse::Flat(items) => items,
        other => panic!("unexpected {other:?}"),
    }
}

fn names(items: &[SymbolInformation]) -> Vec<&str> {
    items.iter().map(|s| s.name.as_str()).collect()
}

#[test]
fn finds_symbols_in_open_documents() {
    let (mut client, _) = Client::start();
    client.open_at(
        &uri("one"),
        "fn alpha(x: i32) -> i32 { x }\nbeta = range(0, 2)\n",
    );
    client.open_at(&uri("two"), "enum Gamma { A }\n");
    let all = query(&mut client, "");
    assert_eq!(all.len(), 3);
    let alpha = query(&mut client, "alp");
    assert_eq!(names(&alpha), ["alpha"]);
    assert_eq!(alpha[0].kind, SymbolKind::FUNCTION);
    assert_eq!(alpha[0].location.uri, uri("one"));
    client.shutdown();
}

#[test]
fn query_is_case_insensitive_substring_then_fuzzy_ranked() {
    let (mut client, _) = Client::start();
    client.open_at(
        &uri("rank"),
        "fn my_total(x: i32) -> i32 { x }\nfn total(x: i32) -> i32 { x }\nfn t_o_t(x: i32) -> i32 { x }\nfn unrelated(x: i32) -> i32 { x }\n",
    );
    let found = query(&mut client, "TOT");
    assert_eq!(names(&found), ["total", "my_total", "t_o_t"]);
    assert!(query(&mut client, "zzz").is_empty());
    client.shutdown();
}

#[test]
fn scans_marigold_files_in_workspace_folders() {
    let dir = scratch();
    fs::write(dir.join("a.marigold"), "fn disk_fn(x: i32) -> i32 { x }\n").unwrap();
    fs::create_dir_all(dir.join("sub/deeper")).unwrap();
    fs::write(dir.join("sub/deeper/b.marigold"), "enum DiskEnum { A }\n").unwrap();
    fs::write(
        dir.join("notes.txt"),
        "fn not_marigold(x: i32) -> i32 { x }\n",
    )
    .unwrap();
    let (mut client, _) = Client::start_with(init_with_folder(&dir));
    let found = query(&mut client, "disk");
    let mut got = names(&found);
    got.sort();
    assert_eq!(got, ["DiskEnum", "disk_fn"]);
    assert!(query(&mut client, "not_marigold").is_empty());
    client.shutdown();
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn skips_target_node_modules_symlinks_and_oversized_files() {
    let dir = scratch();
    fs::write(dir.join("real.marigold"), "fn kept(x: i32) -> i32 { x }\n").unwrap();
    for skipped in ["target", "node_modules"] {
        fs::create_dir_all(dir.join(skipped)).unwrap();
        fs::write(
            dir.join(skipped).join("x.marigold"),
            format!("fn in_{skipped}(x: i32) -> i32 {{ x }}\n"),
        )
        .unwrap();
    }
    let outside = scratch();
    fs::write(
        outside.join("o.marigold"),
        "fn via_link(x: i32) -> i32 { x }\n",
    )
    .unwrap();
    #[cfg(unix)]
    {
        std::os::unix::fs::symlink(outside.join("o.marigold"), dir.join("link.marigold")).unwrap();
        std::os::unix::fs::symlink(&outside, dir.join("linkdir")).unwrap();
    }
    let big = fs::File::create(dir.join("big.marigold")).unwrap();
    big.set_len(marigold_lsp::workspace::MAX_FILE_BYTES + 1)
        .unwrap();
    let (mut client, _) = Client::start_with(init_with_folder(&dir));
    let found = query(&mut client, "");
    assert_eq!(names(&found), ["kept"]);
    client.shutdown();
    fs::remove_dir_all(&dir).unwrap();
    fs::remove_dir_all(&outside).unwrap();
}

#[test]
fn file_count_is_bounded() {
    let dir = scratch();
    for i in 0..(marigold_lsp::workspace::MAX_FILES + 25) {
        fs::write(
            dir.join(format!("f{i:05}.marigold")),
            format!("v{i} = range(0, 1)\n"),
        )
        .unwrap();
    }
    let files = marigold_lsp::workspace::scan(std::slice::from_ref(&dir));
    assert_eq!(files.len(), marigold_lsp::workspace::MAX_FILES);
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn open_document_shadows_disk_contents() {
    let dir = scratch();
    let file = dir.join("a.marigold");
    fs::write(&file, "fn on_disk(x: i32) -> i32 { x }\n").unwrap();
    let (mut client, _) = Client::start_with(init_with_folder(&dir));
    client.open_at(&folder_uri(&file), "fn in_buffer(x: i32) -> i32 { x }\n");
    let found = query(&mut client, "");
    assert_eq!(names(&found), ["in_buffer"]);
    client.shutdown();
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
fn root_uri_is_used_when_no_workspace_folders() {
    let dir = scratch();
    fs::write(
        dir.join("a.marigold"),
        "fn from_root(x: i32) -> i32 { x }\n",
    )
    .unwrap();
    #[allow(deprecated)]
    let params = InitializeParams {
        root_uri: Some(folder_uri(&dir)),
        ..Default::default()
    };
    let (mut client, _) = Client::start_with(params);
    assert_eq!(names(&query(&mut client, "from_root")), ["from_root"]);
    client.shutdown();
    fs::remove_dir_all(&dir).unwrap();
}

#[test]
#[cfg(unix)]
fn percent_encoded_workspace_paths_resolve() {
    let dir = scratch().join("with space");
    fs::create_dir_all(&dir).unwrap();
    fs::write(dir.join("a.marigold"), "fn spaced(x: i32) -> i32 { x }\n").unwrap();
    let encoded = lsp_types::Uri::from_str(&format!(
        "file://{}",
        dir.display().to_string().replace(' ', "%20")
    ))
    .unwrap();
    let params = InitializeParams {
        workspace_folders: Some(vec![WorkspaceFolder {
            uri: encoded,
            name: "ws".into(),
        }]),
        ..Default::default()
    };
    let (mut client, _) = Client::start_with(params);
    assert_eq!(names(&query(&mut client, "spaced")), ["spaced"]);
    client.shutdown();
}

#[test]
fn missing_workspace_root_is_harmless() {
    let params = InitializeParams {
        workspace_folders: Some(vec![WorkspaceFolder {
            uri: lsp_types::Uri::from_str("file:///definitely/not/here").unwrap(),
            name: "ws".into(),
        }]),
        ..Default::default()
    };
    let (mut client, _) = Client::start_with(params);
    assert!(query(&mut client, "").is_empty());
    client.shutdown();
}
