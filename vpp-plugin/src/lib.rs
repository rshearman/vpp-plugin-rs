#![warn(
    missing_docs,
    missing_copy_implementations,
    clippy::undocumented_unsafe_blocks
)]

//! # Framework for writing high-performance VPP plugins in Rust
//!
//! [VPP](https://wiki.fd.io/view/VPP/What_is_VPP%3F) is a high performance packet processing
//! with support for a large number of features, and enables writing plugins to extend its
//! functionality.
//!
//! This crate provides the core functionality for writing VPP plugins in Rust. It includes
//! bindings to the VPP C API, as well as abstractions for writing VPP nodes, with the goal of
//! having performance parity with C plugins for fast path packet processing.

pub mod bindings;
#[doc(hidden)]
pub mod macro_support;
pub mod vlib;
pub mod vppinfra;

// Re-export macros for convenience
pub use vpp_plugin_macros::{vlib_init_function, vlib_plugin_register};
