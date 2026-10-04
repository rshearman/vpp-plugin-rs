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

/// `VPP_BUILD_VER` from the VPP headers being compiled against (`vpp/app/version.h`), read
/// through the C preprocessor so the compiler's own include path decides which VPP it is.
fn vpp_build_ver() -> String {
    let probe = PathBuf::from(env::var("OUT_DIR").unwrap()).join("vpp_build_ver_probe.c");
    std::fs::write(
        &probe,
        "#include <vpp/app/version.h>\nvpp_plugin_build_ver VPP_BUILD_VER\n",
    )
    .expect("write VPP_BUILD_VER probe");
    let expanded = cc::Build::new()
        .file(&probe)
        .cargo_metadata(false)
        .try_expand()
        .expect("preprocess vpp/app/version.h");
    String::from_utf8_lossy(&expanded)
        .lines()
        .find_map(|l| l.trim().strip_prefix("vpp_plugin_build_ver"))
        .map(|v| v.trim().trim_matches('"').to_owned())
        .expect("VPP_BUILD_VER not defined by vpp/app/version.h")
}

fn main() {
    println!("cargo:rerun-if-changed=vlib_wrapper.h");
    println!("cargo:rustc-check-cfg=cfg(pregenerated_bindings)");

    if !headers_available() {
        println!("cargo:rustc-cfg=pregenerated_bindings");
        // No VPP to pin to; an empty `version_required` is no requirement to VPP's loader.
        println!("cargo:rustc-env=VPP_PLUGIN_VPP_BUILD_VER=");
        return;
    }

    println!(
        "cargo:rustc-env=VPP_PLUGIN_VPP_BUILD_VER={}",
        vpp_build_ver()
    );

    build_wrapper();

    let bindings = bindgen::Builder::default()
        .header("vlib_wrapper.h")
        .allowlist_file("vlib_wrapper\\.h")
        .allowlist_file(".*/vlib/buffer\\.h")
        .allowlist_file(".*/vlib/buffer_funcs\\.h")
        .allowlist_file(".*/vlib/cli\\.h")
        .allowlist_file(".*/vlib/config\\.h")
        .allowlist_file(".*/vlib/counter\\.h")
        .allowlist_file(".*/vlib/defs\\.h")
        .allowlist_file(".*/vlib/global_funcs\\.h")
        .allowlist_file(".*/vlib/init\\.h")
        .allowlist_file(".*/vlib/main\\.h")
        .allowlist_file(".*/vlib/node\\.h")
        .allowlist_file(".*/vlib/threads\\.h")
        .allowlist_file(".*/vlib/trace_funcs\\.h")
        .allowlist_file(".*/vlib/unix/plugin\\.h")
        .allowlist_file(".*/vlibapi/api\\.h")
        .allowlist_file(".*/vlibapi/api_common\\.h")
        .allowlist_file(".*/vlibapi/memory_shared\\.h")
        .allowlist_file(".*/vlibmemory/api\\.h")
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
