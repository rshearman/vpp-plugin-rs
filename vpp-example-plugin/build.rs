use std::{env, path::PathBuf};

fn main() {
    let output_dir = PathBuf::from(env::var("OUT_DIR").unwrap()).join("src");
    vpp_plugin_api_gen::Builder::new("example.api", &output_dir.to_string_lossy())
        .expect("unable to generate API binding")
        .generate()
        .expect("unable to generate API binding");
}
