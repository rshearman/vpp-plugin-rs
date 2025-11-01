//! VPP core library
//!
//! This module contains abstractions around VPP's core library, `vppinfra`, including
//! vectors and utilities.

pub mod error;
pub mod vec;

pub use vec::{Vec, VecRef};

/// Unstable API for doctest usage only
#[doc(hidden)]
pub fn clib_mem_init() {
    use std::sync::OnceLock;

    static INIT: OnceLock<()> = OnceLock::new();

    let _ = INIT.get_or_init(|| {
        // SAFETY: it's safe to call this with a null base pointer, since that tells VPP to
        // choose it for us, and there are no constraints on the size paramter. There are no
        // concurrency issues since this just sets the per-thread heap.
        unsafe {
            // 3 << 30 is just the value that the C vppinfra UTs use in vpp
            crate::bindings::clib_mem_init(std::ptr::null_mut(), 3 << 30);
        }
    });
}
