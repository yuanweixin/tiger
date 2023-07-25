load("@crate_index//:defs.bzl", "aliases", "all_crate_deps")
load("@rules_rust//rust:defs.bzl", "rust_library", "rust_binary", "rust_test")
load("@rules_rust//cargo:defs.bzl", "cargo_build_script")


# This will run the build script from the root of the workspace, and
# collect the outputs.
cargo_build_script(
    name = "build_script",
    srcs = ["build.rs"],
    data = ["src/tiger.l", "src/tiger.y"],
    # Optional environment variables passed during build.rs compilation
    # rustc_env = {
    # },
    # Optional environment variables passed during build.rs execution.
    # Note that as the build script's working directory is not execroot,
    # execpath/location will return an absolute path, instead of a relative
    # one.
    # build_script_env = {
    #     "SOME_TOOL_OR_FILE": "$(execpath @tool//:binary)"
    # }
    # Optional data/tool dependencies
    # data = ["@tool//:binary"],
    deps = all_crate_deps(build = True)
)

rust_binary(
    name = "tc",
    srcs = glob(["**/*.rs"]),
    aliases = aliases(),
    # edition = "2021",
    proc_macro_deps = all_crate_deps(proc_macro = True),
    deps = all_crate_deps(normal = True) + [":build_script"]
)