//! Example VPP plugin
//!
//! This example demonstrates a minimal, idiomatic Rust VPP plugin built using
//! the helper macros and types provided by the `vpp_plugin` crate. It is
//! intended as a cookbook-style reference for the following concepts:
//!
//! - Implementing a VPP node with the `#[vlib_node]` macro and the node traits
//!   (`vlib::node::Node`). The node here is a small packet-processing node
//!   that inspects the IPv4 header and either traces/drops (if the packet is
//!   ICMP) or forwards the packet to the next feature (if not).
//! - Using `generic_node_x1` and the `GenericNodeX1` mapping helper for
//!   simpler and less error-prone processing logic.
//! - Defining `NextNodes` (via `#[derive(NextNodes)]`) and signalling the next
//!   action (drop or next feature) from the node.
//! - Capturing trace data with a custom `TraceData` type and a `format_trace`
//!   function to make packet traces human-readable in the VPP CLI.
//! - Declaring error counters with `#[derive(ErrorCounters)]` and recording
//!   errors on buffers (used here for dropped ICMP packets).
//! - Registering a vnet feature using `vnet_feature_init!`, and enabling or
//!   disabling it per-interface at runtime from a CLI command (`rust-example`).
//! - Integrating generated API handlers: the example shows registering message
//!   handlers produced by the API generator
//!   (`example_api::example_register_messages`) and implementing a handler
//!   that returns a `vlibapi::Message<...>` reply.
//!
//! The plugin is intentionally small and focussed and can be used as a
//! template when implementing your own feature.

use vpp_plugin::vlib_plugin_register;

vlib_plugin_register! {
    version: "1.0",
    description: "Example",
}
