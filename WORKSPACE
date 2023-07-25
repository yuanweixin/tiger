workspace(name = "rules_rust_serde")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")
http_archive(
    name = "rules_rust",
    sha256 = "4a9cb4fda6ccd5b5ec393b2e944822a62e050c7c06f1ea41607f14c4fdec57a2",
    urls = ["https://github.com/bazelbuild/rules_rust/releases/download/0.25.1/rules_rust-v0.25.1.tar.gz"],
)

load("@rules_rust//rust:repositories.bzl", "rust_repositories")

rust_repositories(
    edition = "2021",
)

load("@rules_rust//crate_universe:repositories.bzl", "crate_universe_dependencies")

crate_universe_dependencies()

load("@rules_rust//crate_universe:defs.bzl", "crate", "crates_repository")

crates_repository(
    name = "crate_index",
    cargo_lockfile = "//:Cargo.lock",
    # Generate with:
    # CARGO_BAZEL_REPIN=1 bazel sync --only=crate_index
    lockfile = "//:Cargo.Bazel.lock",
    manifests = ["//tc:Cargo.toml"]
 )

load("@crate_index//:defs.bzl", "crate_repositories")

crate_repositories()

