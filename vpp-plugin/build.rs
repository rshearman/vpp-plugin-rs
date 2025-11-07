use std::env;
use std::path::PathBuf;

fn headers_available() -> bool {
    let mut cc = cc::Build::new();
    cc.file("vlib_wrapper.h");
    cc.warnings_into_errors(true);
    if let Err(e) = cc.try_compile("libvlib_wrapper.a") {
        println!(
            "cargo:warning=Compile failed: {}, using pre-generated bindings",
            e
        );
        false
    } else {
        true
    }
}

fn build_wrapper() {
    println!("cargo:rerun-if-changed=vlib_wrapper.c");

    let mut cc = cc::Build::new();
    cc.file("vlib_wrapper.c");
    cc.warnings_into_errors(true);
    cc.compile("libvlib_wrapper.a");
}

fn main() {
    println!("cargo:rerun-if-changed=vlib_wrapper.h");
    println!("cargo:rustc-check-cfg=cfg(pregenerated_bindings)");

    if !headers_available() {
        println!("cargo:rustc-cfg=pregenerated_bindings");
        return;
    }

    build_wrapper();

    let bindings = bindgen::Builder::default()
        .header("vlib_wrapper.h")
        .allowlist_file("vlib_wrapper\\.h")
        .allowlist_file(".*/vlib/buffer\\.h")
        .allowlist_file(".*/vlib/buffer_funcs\\.h")
        .allowlist_file(".*/vlib/cli\\.h")
        .allowlist_file(".*/vlib/config\\.h")
        .allowlist_file(".*/vlib/defs\\.h")
        .allowlist_file(".*/vlib/global_funcs\\.h")
        .allowlist_file(".*/vlib/init\\.h")
        .allowlist_file(".*/vlib/main\\.h")
        .allowlist_file(".*/vlib/node\\.h")
        .allowlist_file(".*/vlib/trace_funcs\\.h")
        .allowlist_file(".*/vlib/unix/plugin\\.h")
        .allowlist_file(".*/vlibapi/api\\.h")
        .allowlist_file(".*/vlibapi/api_common\\.h")
        .allowlist_file(".*/vlibapi/memory_shared\\.h")
        .allowlist_file(".*/vnet/buffer\\.h")
        .allowlist_file(".*/vnet/error\\.h")
        .allowlist_file(".*/vnet/feature/feature\\.h")
        .allowlist_file(".*/vnet/global_funcs\\.h")
        .allowlist_file(".*/vppinfra/config\\.h")
        .allowlist_file(".*/vppinfra/error\\.h")
        .allowlist_file(".*/vppinfra/error_bootstrap\\.h")
        .allowlist_file(".*/vppinfra/format\\.h")
        .allowlist_file(".*/vppinfra/mem\\.h")
        .allowlist_file(".*/vppinfra/vec\\.h")
        .allowlist_file(".*/vppinfra/vec_bootstrap\\.h")
        // allowing .*/vnet/feature/feature\\.h causes errors in generated code due to duplicate definitions
        .allowlist_item("feature_main")
        .allowlist_type("ip4_header_t")
        .allowlist_type("ip6_header_t")
        .allowlist_type("format_ip4_header")
        .allowlist_type("format_ip6_header")
        .allowlist_type("format_vnet_sw_if_index_name")
        // bindgen generates duplicate definitions for these types due to forward declarations in vlib/trace.h
        .blocklist_type("vlib_trace_main_t")
        .blocklist_type("vlib_buffer_t")
        // bindgen generates duplicate definitions for these types due to forward declarations in vnet/interface.h, so we blocklist them and declare them manually here
        .blocklist_type("vnet_sw_interface_t")
        // Avoid bindgen-generated code causing this to have an alignment of 8 instead of 1
        .blocklist_type("ip6_address_t")
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
