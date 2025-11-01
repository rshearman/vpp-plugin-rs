use std::env;
use std::path::PathBuf;

fn build_wrapper() {
    println!("cargo:rerun-if-changed=vlib_wrapper.c");

    let mut cc = cc::Build::new();
    cc.file("vlib_wrapper.c");
    cc.warnings_into_errors(true);
    cc.compile("libvlib_wrapper.a");
}

fn main() {
    build_wrapper();

    println!("cargo:rerun-if-changed=vlib_wrapper.h");

    let bindings = bindgen::Builder::default()
        .header("vlib_wrapper.h")
        .allowlist_file("vlib_wrapper\\.h")
        .allowlist_file(".*/vlib/init\\.h")
        .allowlist_file(".*/vlib/main\\.h")
        .allowlist_file(".*/vlib/unix/plugin\\.h")
        .allowlist_file(".*/vppinfra/error\\.h")
        .allowlist_file(".*/vppinfra/error_bootstrap\\.h")
        .allowlist_file(".*/vppinfra/mem\\.h")
        .allowlist_file(".*/vppinfra/vec\\.h")
        .allowlist_file(".*/vppinfra/vec_bootstrap\\.h")
        // bindgen generates duplicate definitions for these types due to forward declarations in vlib/trace.h
        .blocklist_type("vlib_trace_main_t")
        .blocklist_type("vlib_buffer_t")
        .flexarray_dst(true)
        .derive_default(true)
        .layout_tests(false)
        // Can cause include path to not match compiler when multiple versions of clang installed
        .detect_include_paths(false)
        .generate()
        .expect("Unable to generate bindings");

    let out_path = PathBuf::from(env::var("OUT_DIR").unwrap());
    bindings
        .write_to_file(out_path.join("vlib_bindings.rs"))
        .expect("Couldn't write vlib_bindings!");

    println!("cargo:rustc-link-lib=vlibmemory");
    println!("cargo:rustc-link-lib=vnet");
    println!("cargo:rustc-link-lib=vlibapi");
    println!("cargo:rustc-link-lib=vlib");
    println!("cargo:rustc-link-lib=vppinfra");
}
