use std::{env, path::PathBuf};

fn main() {
    let output_dir = PathBuf::from(env::var("OUT_DIR").unwrap()).join("src");
    vpp_plugin_api_gen::Builder::new("test.api", &output_dir.to_string_lossy())
        .expect("unable to generate API binding")
        .generate()
        .expect("unable to generate API binding");
    vpp_plugin_api_gen::Builder::new("test_types.api", &output_dir.to_string_lossy())
        .expect("unable to generate API binding")
        .generate()
        .expect("unable to generate API binding");
    vpp_plugin_api_gen::Builder::new("test_tagged_union.api", &output_dir.to_string_lossy())
        .expect("unable to generate API binding")
        .generate()
        .expect("unable to generate API binding");
}
