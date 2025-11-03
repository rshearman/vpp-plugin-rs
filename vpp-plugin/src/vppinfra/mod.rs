//! VPP core library
//!
//! This module contains abstractions around VPP's core library, `vppinfra`, including
//! vectors and utilities.

pub mod cache;
pub mod error;
pub mod unformat;
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

/// Hints to the compiler that the given code path is cold, i.e., unlikely to be executed.
// until std::hint::cold_path is stabilised
#[cold]
pub const fn cold_path() {}

/// Hints to the compiler that the given boolean expression is likely to be true
///
/// See also [`cold_path`] and [`unlikely`].
// until std::hint::likely is stabilised
#[inline(always)]
pub const fn likely(b: bool) -> bool {
    if b {
        true
    } else {
        cold_path();
        false
    }
}

/// Hints to the compiler that the given boolean expression is unlikely to be true
///
/// See also [`likely`].
// until std::hint::unlikely is stabilised
#[inline(always)]
pub const fn unlikely(b: bool) -> bool {
    if b {
        cold_path();
        true
    } else {
        false
    }
}

/// Compile-time assertion
///
/// Evaluates the given expression at compile time, causing a compilation error if it
/// evaluates to false.
#[macro_export]
macro_rules! const_assert {
    ($x:expr $(,)?) => {
        #[allow(unknown_lints, eq_op)]
        const _: [(); 0 - !{
            const ASSERT: bool = $x;
            ASSERT
        } as usize] = [];
    };
}
