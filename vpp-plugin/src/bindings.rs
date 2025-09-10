// Allow all sorts of warnings triggered by bindgen-generated code
#![allow(
    clippy::all,
    clippy::undocumented_unsafe_blocks,
    missing_docs,
    non_camel_case_types,
    non_upper_case_globals,
    non_snake_case,
    improper_ctypes,
    unreachable_pub,
    unsafe_op_in_unsafe_fn,
    missing_copy_implementations
)]

//! Bindings to VPP's C API
//!
//! These are low-level bindings to VPP's C API and are primarily intended for internal use by
//! higher-level abstractions in this crate. Direct use of these bindings is generally discouraged
//! unless absolutely necessary, as they are unsafe and do not provide any safety guarantees.
//!
//! This isn't intended to be a complete set of bindings to all of VPP's C API, only the parts that are
//! necessary for implementing the higher-level abstractions in this crate. They should be
//! considered an unstable part of the API and, as such, bindings may be removed over time without
//! incrementing the major version number or prior deprecation.
//!
//! Note that these bindings may vary between VPP versions and targets, and care should be taken
//! to ensure compatibility when using them directly.

include!(concat!(env!("OUT_DIR"), "/vlib_bindings.rs"));
