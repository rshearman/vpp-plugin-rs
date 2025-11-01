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

// bindgen generates duplicate definitions for these types due to forward declarations in vlib/trace.h, so we blocklist them and declare them manually here

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct vlib_trace_main_t {
    pub trace_buffer_pool: *mut *mut vlib_trace_header_t,
    pub last_main_loop_count: u32_,
    pub filter_node_index: u32_,
    pub filter_flag: u32_,
    pub filter_count: u32_,
    pub trace_enable: u32_,
    pub nodes: *mut vlib_trace_node_t,
    pub verbose: ::std::os::raw::c_int,
    pub trace_buffer_callback: vlib_trace_buffer_callback_t,
    pub add_trace_callback: vlib_add_trace_callback_t,
    pub current_trace_filter_function: vlib_is_packet_traced_fn_t,
}

#[repr(C)]
#[repr(align(64))]
#[derive(Copy, Clone)]
pub struct vlib_buffer_t {
    pub cacheline0: __BindgenUnionField<[u8_; 0usize]>,
    pub __bindgen_anon_1: __BindgenUnionField<vlib_buffer_t__bindgen_ty_1>,
    pub bindgen_union_field: [u8; 256usize],
}
