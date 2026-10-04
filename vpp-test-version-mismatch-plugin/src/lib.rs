//! Test VPP plugin that requires a VPP build that does not exist
//!
//! VPP must refuse to load it; if it were loaded, its CLI command would exist.

use vpp_plugin::{vlib, vlib_cli_command, vlib_plugin_register, vppinfra::error::ErrorStack};

#[vlib_cli_command(
    path = "rust-test version-mismatch",
    short_help = "rust-test version-mismatch"
)]
fn loaded_command(_vm: &mut vlib::BarrierHeldMainRef, _input: &str) -> Result<(), ErrorStack> {
    Ok(())
}

vlib_plugin_register! {
    version: "1.0",
    description: "Test version mismatch",
    version_required: "0.0-no-such-vpp-build",
}
