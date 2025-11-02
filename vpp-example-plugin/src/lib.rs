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

use std::fmt;

use vpp_plugin::{
    bindings::ip4_header_t,
    vlib::{
        self,
        node_generic::{generic_feature_node_x1, FeatureNextNode, GenericFeatureNodeX1},
        BufferIndex,
    },
    vlib_init_function, vlib_node, vlib_plugin_register,
    vppinfra::{error::ErrorStack, unlikely},
    ErrorCounters, NextNodes,
};

const IP_PROTOCOL_ICMP: u8 = 1;

#[derive(Copy, Clone)]
struct ExampleTrace {
    header: ip4_header_t,
}

impl fmt::Display for ExampleTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "header: {}", self.header)
    }
}

fn format_example_trace(
    _vm: &mut vlib::MainRef,
    _node: &mut vlib::NodeRef<ExampleNode>,
    t: &ExampleTrace,
) -> String {
    t.to_string()
}

#[derive(NextNodes)]
enum ExampleNextNode {
    #[next_node = "drop"]
    Drop,
}

#[derive(ErrorCounters)]
enum ExampleErrorCounter {
    #[error_counter(description = "Drops", severity = ERROR)]
    Drop,
}

static EXAMPLE_NODE: ExampleNode = ExampleNode::new();

#[vlib_node(
    name = "example",
    instance = EXAMPLE_NODE,
    format_trace = format_example_trace,
)]
struct ExampleNode;

impl ExampleNode {
    const fn new() -> Self {
        Self
    }
}

impl vlib::node::Node for ExampleNode {
    type Vector = BufferIndex;
    type Scalar = ();
    type Aux = ();

    type NextNodes = ExampleNextNode;
    type RuntimeData = ();
    type TraceData = ExampleTrace;
    type Errors = ExampleErrorCounter;
    type FeatureData = ();

    #[inline(always)]
    unsafe fn function(
        &self,
        vm: &mut vlib::MainRef,
        node: &mut vlib::NodeRuntimeRef<Self>,
        frame: &mut vlib::FrameRef<Self>,
    ) -> u16 {
        struct Impl;
        impl GenericFeatureNodeX1<ExampleNode> for Impl {
            #[inline(always)]
            unsafe fn map_buffer_to_next(
                &self,
                vm: &vlib::MainRef,
                node: &mut vlib::NodeRuntimeRef<ExampleNode>,
                b0: &mut vlib::BufferRef<()>,
            ) -> FeatureNextNode<ExampleNextNode> {
                let ip: *const ip4_header_t = b0.current_ptr_mut() as *const ip4_header_t;
                if (*ip).__bindgen_anon_1.protocol == IP_PROTOCOL_ICMP {
                    b0.set_error(node, ExampleErrorCounter::Drop);
                    if unlikely(b0.flags().contains(vlib::BufferFlags::IS_TRACED)) {
                        let t = b0.add_trace(vm, node);
                        t.write(ExampleTrace { header: *ip });
                    }
                    ExampleNextNode::Drop.into()
                } else {
                    FeatureNextNode::NextFeature
                }
            }
        }
        generic_feature_node_x1(vm, node, frame, Impl)
    }
}

#[vlib_init_function]
fn example_init(_vm: &mut vlib::BarrierHeldMainRef) -> Result<(), ErrorStack> {
    Ok(())
}

vlib_plugin_register! {
    version: "1.0",
    description: "Example",
}
