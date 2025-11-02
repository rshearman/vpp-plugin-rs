//! Cache utilities and definitions
//!
//! This module provides functions for prefetching memory locations into CPU caches.

/// Prefetches a memory location into L1 cache for reading
// Instead of std::intrinsics::prefetch_read_data which is nightly-only
#[inline(always)]
pub fn prefetch_load<T>(p: *const T) {
    #[cfg(target_arch = "x86_64")]
    // SAFETY: it's not clear why this is marked as unsafe, as prefetching a pointer
    // doesn't dereference it or read from it.
    unsafe {
        std::arch::x86_64::_mm_prefetch::<{ std::arch::x86_64::_MM_HINT_T0 }>(p as *const i8);
    }
}

/// Prefetches a memory location into L1 cache for writing
// Instead of std::intrinsics::prefetch_write_data which is nightly-only
#[inline(always)]
pub fn prefetch_store<T>(p: *const T) {
    #[cfg(target_arch = "x86_64")]
    // SAFETY: it's not clear why this is marked as unsafe, as prefetching a pointer
    // doesn't dereference it or write to it.
    unsafe {
        std::arch::x86_64::_mm_prefetch::<{ std::arch::x86_64::_MM_HINT_ET0 }>(p as *const i8);
    }
}
