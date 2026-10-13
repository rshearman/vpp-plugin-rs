//! VPP vlib main structure wrapper
//!
//! This module contains abstractions around VPP's `vlib_main_t` structure.
//! It provides safe access to the main VPP library context.

use std::{marker::PhantomData, ops::Deref, panic::Location};

use crate::bindings::{
    vlib_main_t, vlib_worker_thread_barrier_release, vlib_worker_thread_barrier_sync_int,
};

pub mod sync;

/// Reference to VPP per-thread main library context
///
/// This is a safe wrapper around `vlib_main_t`, providing access to its fields.
///
/// A `&mut MainRef` is equivalent to a `vlib_main_t *` in C (a `*mut vlib_main_t` in Rust).
///
/// See also [`BarrierHeldMainRef`] for a variant that indicates the caller holds the
/// main thread barrier.
#[repr(transparent)]
pub struct MainRef(foreign_types::Opaque);

impl MainRef {
    /// Creates a `&mut MainRef` directly from a pointer
    ///
    /// # Safety
    /// - The pointer must be valid and a properly initialised `vlib_main_t` for the thread.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    /// - The `vlib_main_t` must have been created by VPP at startup and not manually by the
    ///   caller, as many datastructures size the memory they allocate by the number of threads
    ///   at startup and are subsequently indexed by thread index.
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_main_t) -> &'a mut Self {
        // SAFETY: caller must ensure the safety requirements are met
        unsafe { &mut *(ptr as *mut _) }
    }

    /// Returns the raw pointer to the underlying `vlib_main_t`
    pub fn as_ptr(&self) -> *mut vlib_main_t {
        self as *const _ as *mut _
    }

    /// Returns the 0-based thread index of this VPP main context
    pub fn thread_index(&self) -> u16 {
        // SAFETY: as long as `self` is a valid `MainRef`, so is the pointer dereference
        unsafe { (*self.as_ptr()).thread_index }
    }

    /// Takes the VPP worker thread barrier, stopping all worker threads
    ///
    /// Returns an RAII guard which releases the barrier when dropped and which dereferences to a
    /// [`BarrierHeldMainRef`] for use with APIs that require the barrier to be held.
    ///
    /// The barrier is recursive in VPP, so it is fine to call this when the barrier is already
    /// held. If there are no worker threads then this is cheap.
    ///
    /// The returned guard must not be held across an async await.
    ///
    /// # Panics
    ///
    /// Panics if not called on the VPP main thread.
    #[track_caller]
    pub fn barrier_sync(&self) -> BarrierGuard<'_> {
        assert_eq!(
            self.thread_index(),
            0,
            "Barrier can only be taken on the VPP main thread"
        );
        // SAFETY: `self` is a valid main thread `vlib_main_t` and the file name is a
        // `'static` nul-terminated string, since VPP stores the pointer for debugging/elog.
        unsafe {
            vlib_worker_thread_barrier_sync_int(
                self.as_ptr(),
                Location::caller().file_as_c_str().as_ptr(),
            );
        }
        BarrierGuard {
            vm: self,
            _phantom: PhantomData,
        }
    }
}

/// RAII guard for the VPP worker thread barrier, returned by [`MainRef::barrier_sync`]
///
/// The barrier is released when this guard is dropped. Leaking the guard (e.g. via
/// [`std::mem::forget`]) leaves the barrier held, which stalls workers but isn't unsound.
#[must_use = "if unused the barrier will immediately be released"]
pub struct BarrierGuard<'vm> {
    vm: &'vm MainRef,
    /// Prevent the type from being `Send` or `Sync`: the barrier must be released on the main
    /// thread. Since `MainRef` isn't currently `Sync` this is defensive in case that changes in
    /// the future.
    _phantom: PhantomData<*mut ()>,
}

impl Deref for BarrierGuard<'_> {
    type Target = BarrierHeldMainRef;

    fn deref(&self) -> &Self::Target {
        // SAFETY: the barrier is held for the lifetime of the guard, `BarrierHeldMainRef` is
        // `repr(transparent)` over `MainRef`, and we only hand out a shared reference.
        unsafe { &*(self.vm as *const MainRef as *const BarrierHeldMainRef) }
    }
}

impl Drop for BarrierGuard<'_> {
    fn drop(&mut self) {
        // SAFETY: the barrier was taken by `barrier_sync` on this (main) thread and the guard
        // can't have moved to another thread.
        unsafe { vlib_worker_thread_barrier_release(self.vm.as_ptr()) }
    }
}

/// Reference to VPP per-thread main library context with barrier held
///
/// This is a safe wrapper around `vlib_main_t`, providing access to its fields.
///
/// A `&mut BarrierHeldMainRef` is equivalent to a `vlib_main_t *` in C (a `*mut vlib_main_t` in
/// Rust).
///
/// See also [`MainRef`] for a variant without the constraint that the caller holds the
/// main thread barrier.
#[repr(transparent)]
pub struct BarrierHeldMainRef(MainRef);

impl BarrierHeldMainRef {
    /// Creates a `&mut BarrierHeldMainRef` directly from a pointer
    ///
    /// # Safety
    /// - All the safety requirements of [`MainRef::from_ptr_mut`] apply.
    /// - Additionally, the caller must that no other thread is concurrently accessing VPP data
    ///   structures. This is typically ensured by holding the main thread barrier, but also
    ///   applies during initialisation before worker threads are started or shutdown after
    ///   worker threads are stopped.
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_main_t) -> &'a mut Self {
        // SAFETY: caller must ensure the safety requirements are met, plus `BarrierHeldMainRef`
        // is `repr(transparent)` over `MainRef`.
        unsafe {
            // Sanity check: barrier should only ever be held on main thread
            debug_assert!((*ptr).thread_index == 0);
            &mut *(ptr as *mut _)
        }
    }
}

impl Deref for BarrierHeldMainRef {
    type Target = MainRef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
