//! VPP application library
//!
//! This module contains abstractions around VPP's application layer, `vlib`.

pub mod buffer;
pub mod main;
pub mod node;
pub mod node_generic;

pub use buffer::{BufferFlags, BufferIndex, BufferRef};
pub use main::{BarrierHeldMainRef, MainRef};
pub use node::{FrameRef, NodeRef, NodeRuntimeRef};
