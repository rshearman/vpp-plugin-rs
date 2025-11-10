//! VPP vlib main structure wrapper
//!
//! This module contains abstractions around VPP's `vlib_main_t` structure.
//! It provides safe access to the main VPP library context.

use std::ops::Deref;

use crate::bindings::vlib_main_t;

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
        // Sanity check: barrier should only ever be held on main thread
        debug_assert!((*ptr).thread_index == 0);
        // SAFETY: caller must ensure the safety requirements are met
        unsafe { &mut *(ptr as *mut _) }
    }
}

impl Deref for BarrierHeldMainRef {
    type Target = MainRef;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
