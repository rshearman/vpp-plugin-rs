use std::{env, path::PathBuf};

/// Directory containing a copy of the VPP API files used
///
/// A copy is always used, rather than those of any installed VPP, so that the generated code is
/// the same in every environment, including those without VPP installed (e.g. for clippy).
const VPP_API_DIR: &str = "vpp-api";

fn main() {
    let output_dir = PathBuf::from(env::var("OUT_DIR").unwrap()).join("src");

    vpp_plugin_api_gen::Builder::new(
        &format!("{VPP_API_DIR}/vnet/ip/ip_types.api"),
        &output_dir.to_string_lossy(),
    )
    .expect("unable to generate API binding")
    .generate()
    .expect("unable to generate API binding");

    for api_file in ["test.api", "test_types.api", "test_tagged_union.api"] {
        vpp_plugin_api_gen::Builder::new(api_file, &output_dir.to_string_lossy())
            .expect("unable to generate API binding")
            .include_dir(".")
            .include_dir(VPP_API_DIR)
            .generate()
            .expect("unable to generate API binding");
    }
}
