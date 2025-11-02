//! vlib buffer abstraction
//!
//! This module contains abstractions around VPP's `vlib_buffer_t` structure.
//! It provides safe access to buffer fields and operations.
//! It also includes buffer allocation and deallocation functions.
//! The goal is to provide a safe and ergonomic interface for working with VPP buffers.

use std::mem::MaybeUninit;

use arrayvec::ArrayVec;
use bitflags::bitflags;

use crate::{
    bindings::{
        vlib_add_trace, vlib_buffer_func_main, vlib_buffer_t, vlib_buffer_t__bindgen_ty_1,
        vlib_buffer_t__bindgen_ty_1__bindgen_ty_1__bindgen_ty_1,
        vlib_helper_feature_next_with_data, CLIB_LOG2_CACHE_LINE_BYTES, VLIB_BUFFER_EXT_HDR_VALID,
        VLIB_BUFFER_IS_TRACED, VLIB_BUFFER_MIN_CHAIN_SEG_SIZE, VLIB_BUFFER_NEXT_PRESENT,
        VLIB_BUFFER_PRE_DATA_SIZE, VLIB_BUFFER_TOTAL_LENGTH_VALID,
    },
    vlib::{
        node::{ErrorCounters, Node, NodeRuntimeRef, VectorBufferIndex},
        MainRef,
    },
};

#[cfg(feature = "experimental")]
use crate::bindings::{vlib_helper_buffer_alloc, vlib_helper_buffer_free};
#[cfg(feature = "experimental")]
use std::fmt;

/// VPP buffer index
#[repr(transparent)]
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BufferIndex(u32);

impl BufferIndex {
    /// Construct a new `BufferIndex`
    pub const fn new(buffer: u32) -> Self {
        Self(buffer)
    }
}

impl From<u32> for BufferIndex {
    fn from(value: u32) -> BufferIndex {
        Self(value)
    }
}

impl From<BufferIndex> for u32 {
    fn from(value: BufferIndex) -> Self {
        value.0
    }
}

impl VectorBufferIndex for BufferIndex {
    fn as_u32_slice(slice: &[Self]) -> &[u32] {
        // SAFETY: BufferIndex is a repr(transparent) wrapper around u32 so the src and dst slice
        // types have the same memory layout
        unsafe { std::mem::transmute::<&[BufferIndex], &[u32]>(slice) }
    }
}

bitflags! {
    /// vlib buffer flags
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct BufferFlags: u32 {
        /// Trace this buffer
        const IS_TRACED = VLIB_BUFFER_IS_TRACED;
        /// This is one buffer in a chain of buffers
        const NEXT_PRESENT = VLIB_BUFFER_NEXT_PRESENT;
        /// Total length is valid
        const TOTAL_LENGTH_VALID = VLIB_BUFFER_TOTAL_LENGTH_VALID;
        /// Contains external buffer manager header
        const EXT_HDR_VALID = VLIB_BUFFER_EXT_HDR_VALID;

        // Flags can be extended by user/vnet
        const _ = !0;
    }
}

/// Construct a user buffer flag
///
/// `n` must be less than 29 and greater than 0.
pub const fn vlib_buffer_flag_user(n: u32) -> u32 {
    assert!(n < 29 && n > 0);
    1 << (32 - n)
}

/// Reference to a VPP buffer
///
/// A `&mut BufferRef<FeatureData>` is equivalent to a `vlib_buffer_t *` in C (a `*mut
/// vlib_buffer_t` in Rust).
#[repr(transparent)]
pub struct BufferRef<FeatureData>(foreign_types::Opaque, std::marker::PhantomData<FeatureData>);

impl<FeatureData> BufferRef<FeatureData> {
    /// Create a `&BufferRef` from a raw pointer
    ///
    /// # Safety
    ///
    /// - The pointer must be a valid and properly initialised `vlib_buffer_t`.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    #[inline(always)]
    pub unsafe fn from_ptr<'a>(ptr: *mut vlib_buffer_t) -> &'a Self {
        &*(ptr as *mut _)
    }

    /// Create a `&mut BufferRef` from a raw pointer
    ///
    /// # Safety
    ///
    /// - The pointer must be a valid and properly initialised `vlib_buffer_t`.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    #[inline(always)]
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_buffer_t) -> &'a mut Self {
        &mut *(ptr as *mut _)
    }

    /// Returns the raw pointer to the underlying `vlib_buffer_t`
    pub fn as_ptr(&self) -> *mut vlib_buffer_t {
        self as *const _ as *mut _
    }

    fn as_details(&self) -> &vlib_buffer_t__bindgen_ty_1 {
        // SAFETY: since the reference to self is valid, so must be the pointer and it's safe to
        // use the __bindgen_anon_1 union arm since the union is just present to force alignment
        // Creation preconditions mean there are no aliased accesses to the buffer so it's fine
        // to take a reference
        unsafe { (*self.as_ptr()).__bindgen_anon_1.as_ref() }
    }

    fn as_details_mut(&mut self) -> &mut vlib_buffer_t__bindgen_ty_1 {
        // SAFETY: since the reference to self is valid, so must be the pointer and it's safe to
        // use the __bindgen_anon_1 union arm since the union is just present to force alignment.
        // Creation preconditions mean there are no aliased accesses to the buffer so it's fine
        // to take a reference
        unsafe { (*self.as_ptr()).__bindgen_anon_1.as_mut() }
    }

    fn as_metadata(&self) -> &vlib_buffer_t__bindgen_ty_1__bindgen_ty_1__bindgen_ty_1 {
        // SAFETY: since the reference to self is valid, so must be the pointer and it's safe to
        // use the __bindgen_anon_1 union arm since the union is just present to force alignment
        // Creation preconditions mean there are no aliased accesses to the buffer so it's fine
        // to take a reference
        unsafe { self.as_details().__bindgen_anon_1.__bindgen_anon_1.as_ref() }
    }

    fn as_metadata_mut(&mut self) -> &mut vlib_buffer_t__bindgen_ty_1__bindgen_ty_1__bindgen_ty_1 {
        // SAFETY: since the reference to self is valid, so must be the pointer and it's safe to
        // use the __bindgen_anon_1 union arm since the union is just present to force alignment.
        // Creation preconditions mean there are no aliased accesses to the buffer so it's fine
        // to take a reference
        unsafe {
            self.as_details_mut()
                .__bindgen_anon_1
                .__bindgen_anon_1
                .as_mut()
        }
    }

    fn data(&self) -> *const u8 {
        self.as_details().data.as_ptr()
    }

    fn current_data_offset(&self) -> i16 {
        self.as_metadata().current_data
    }

    fn current_data_offset_mut(&mut self) -> &mut i16 {
        &mut self.as_metadata_mut().current_data
    }

    /// Current length
    ///
    /// Typically, this is the amount of packet data remaining from [`Self::current_ptr_mut`].
    pub fn current_length(&self) -> u16 {
        self.as_metadata().current_length
    }

    fn current_length_mut(&mut self) -> &mut u16 {
        &mut self.as_metadata_mut().current_length
    }

    /// Buffer flags
    pub fn flags(&self) -> BufferFlags {
        BufferFlags::from_bits_retain(self.as_metadata().flags)
    }

    /// Get a pointer to the current data
    ///
    /// This corresponds to the VPP C API `vlib_buffer_get_current`.
    ///
    /// # Usage guidance
    ///
    /// Note that the pointer returned may point to uninitialised data depending on the context.
    /// In addition, depending on the context, the remaining data the amount is expected.
    /// Finally, remaining data is sufficent and it's initialised it may not have been validated
    /// so care must be taken in determining whether or not lengths in the headers can be trusted.
    pub fn current_ptr_mut(&mut self) -> *mut u8 {
        let data = self.data().cast_mut();
        let current_data = self.current_data_offset();

        debug_assert!(current_data >= -(VLIB_BUFFER_PRE_DATA_SIZE as i16));

        // SAFETY: current_data is asserted to be valid and point into valid (but possibly
        // unintialised) data or pre_data.
        unsafe {
            if current_data >= 0 {
                data.add(current_data as usize)
            } else {
                data.sub(-current_data as usize)
            }
        }
    }

    // vlib_buffer_has_space
    /// Check if the buffer has space for `l` more bytes
    pub fn has_space(&self, l: i16) -> bool {
        self.current_length() >= l as u16
    }

    // vlib_buffer_advance
    /// Advance the current data pointer by `l` bytes
    ///
    /// # Safety
    ///
    /// - If `l` is positive, the buffer must have at least `l` bytes of data remaining.
    /// - If `l` is negative, the current data offset must be at least `-l` bytes from the start of
    ///   the buffer's data area (including pre-data).
    pub unsafe fn advance(&mut self, l: i16) {
        debug_assert!(l < 0 || self.current_length() >= l as u16);
        debug_assert!(
            l >= 0 || self.current_data_offset() + VLIB_BUFFER_PRE_DATA_SIZE as i16 >= -l
        );

        *self.current_data_offset_mut() += l;
        if l >= 0 {
            *self.current_length_mut() -= l as u16;
        } else {
            *self.current_length_mut() += -l as u16;
        }

        debug_assert!(
            !self.flags().contains(BufferFlags::NEXT_PRESENT)
                || self.current_length() >= VLIB_BUFFER_MIN_CHAIN_SEG_SIZE as u16
        );
    }

    /// Get a pointer to the end of the current data
    ///
    /// This corresponds to the VPP C function `vlib_buffer_get_tail`.
    pub fn tail_mut(&mut self) -> *mut u8 {
        let data = self.data().cast_mut();
        let current_data = self.current_data_offset();

        debug_assert!(current_data >= -(VLIB_BUFFER_PRE_DATA_SIZE as i16));

        // SAFETY: current_data and current_length are asserted to be valid and `current_data +
        // current_length` asserted to point to the end of valid (but possibly unintialised) data
        // or pre_data.
        unsafe {
            let ptr = if current_data >= 0 {
                data.add(current_data as usize)
            } else {
                data.sub(-current_data as usize)
            };
            ptr.add(self.current_length() as usize)
        }
    }

    /// Add trace data to this buffer
    pub fn add_trace<N: Node>(
        &mut self,
        vm: &MainRef,
        node: &NodeRuntimeRef<N>,
    ) -> &mut MaybeUninit<N::TraceData> {
        // SAFETY: pointers are valid and the uninitialised data that is returned cannot be read
        // by safe code
        unsafe {
            &mut *(vlib_add_trace(
                vm.as_ptr(),
                node.as_ptr(),
                self.as_ptr(),
                std::mem::size_of::<N::TraceData>() as u32,
            ) as *mut MaybeUninit<N::TraceData>)
        }
    }

    /// Set an error reason
    ///
    /// This is typically done before sending the packet to the `drop` node, where it use the
    /// value to display the reason in traces and automatically increment the per-node, per-error
    /// counter for the error.
    pub fn set_error<N: Node>(&mut self, node: &NodeRuntimeRef<N>, error: N::Errors) {
        // SAFETY: vlib_node_runtime_t::errors is sized according to the number of error values
        // and it is a precondition of the Errors trait that the value return by into_u16() cannot
        // be greater than or equal to the declared number of error values.
        unsafe {
            let error_value = (*node.as_ptr()).errors.add(error.into_u16() as usize);
            self.as_metadata_mut().error = *error_value;
        }
    }
}

impl<FeatureData: Copy> BufferRef<FeatureData> {
    /// Get the next feature node and feature data for this buffer
    ///
    /// Used when continuing to the next feature node in a node invoked from a feature arc.
    pub fn vnet_feature_next(&self) -> (u32, FeatureData) {
        // SAFETY: we have a reference to self so the pointer must be valid, the number of bytes
        // requested matches the size of the FeatureData type and
        // `vlib_helper_feature_next_with_data` cannot fail.
        unsafe {
            let mut next = MaybeUninit::uninit();
            // TODO: this is in hot path, so optimise for different micro-architectures and don't just do one buffer at a time
            let feature_data = vlib_helper_feature_next_with_data(
                next.as_mut_ptr(),
                self.as_ptr(),
                std::mem::size_of::<FeatureData>() as u32,
            );
            (next.assume_init(), *(feature_data as *const FeatureData))
        }
    }
}

/// Owned buffer (with context)
///
/// The `&MainRef` context is necessary to be able to free the buffer on drop.
#[cfg(feature = "experimental")]
pub struct BufferWithContext<'a> {
    buffer: u32,
    vm: &'a MainRef,
}

#[cfg(feature = "experimental")]
impl<'a> BufferWithContext<'a> {
    /// Creates a `BufferWithContext` directly from a buffer index and a main reference
    ///
    /// # Safety
    /// - The buffer index must be valid and the caller must have ownership of the buffer it
    ///   corresponds to.
    pub unsafe fn from_parts(buffer: u32, vm: &'a MainRef) -> Self {
        Self { buffer, vm }
    }

    /// Get a mutable reference to the buffer
    pub fn as_buffer_ref(&mut self) -> &mut BufferRef<()> {
        let from = &[self.buffer];
        let mut b: ArrayVec<_, 1> = ArrayVec::new();
        // SAFETY: capacity of b equals the length of from, `self.buffer` is a valid index and we
        // force FeatureData to `()` since it isn't known and the buffer cannot be part of a
        // feature arc.
        unsafe {
            self.vm.get_buffers(from, &mut b);
        }
        b.remove(0)
    }
}

#[cfg(feature = "experimental")]
impl Drop for BufferWithContext<'_> {
    fn drop(&mut self) {
        // SAFETY: we have a reference to MainRef so the pointer must be valid, we pass in a
        // pointer to buffers consistent with the number of buffers passed in, and self.buffer
        // is a valid buffer index that we have ownership of.
        unsafe {
            vlib_helper_buffer_free(self.vm.as_ptr(), &mut self.buffer, 1);
        }
    }
}

/// Buffer allocation error
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[cfg(feature = "experimental")]
pub struct BufferAllocError;

#[cfg(feature = "experimental")]
impl fmt::Display for BufferAllocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "buffer allocation error")
    }
}

#[cfg(feature = "experimental")]
impl std::error::Error for BufferAllocError {}

impl MainRef {
    /// Get pointers to buffers for the given buffer indices, writing them into the provided `to` arrayvec.
    ///
    /// This is similar to `vlib_get_buffers` in the C API.
    ///
    /// Note that although it would be more idiomatic to return an `ArrayVec` directly, this
    /// method takes a mutable reference to an `ArrayVec` to avoid an unnecessary copy when
    /// returning.
    ///
    /// # Safety
    ///
    /// - The caller must ensure that `to` has enough capacity to hold all the buffers
    ///   corresponding to the indices in `from_indices`.
    /// - Each index in `from_indices` must be valid and the caller must have ownership of the
    ///   buffer it corresponds to.
    /// - Each buffer's `feature_arc_index` and `current_config_index` must be consistent with
    ///   the `FeatureData` type. If they are not known (i.e. because the caller the node isn't
    ///   being executed in a feature arc), FeatureData should be a zero-sized type such as `()`.
    #[inline(always)]
    pub unsafe fn get_buffers<'a, 'me, 'buf: 'me, FeatureData, const N: usize>(
        &'me self,
        from_indices: &'a [u32],
        to: &mut ArrayVec<&'buf mut BufferRef<FeatureData>, N>,
    ) {
        debug_assert!(from_indices.len() <= N);

        let buffer_mem_start = (*(*self.as_ptr()).buffer_main).buffer_mem_start;
        let base = buffer_mem_start as *const i8;
        for from_index in from_indices.iter() {
            let ptr = base.add((*from_index << CLIB_LOG2_CACHE_LINE_BYTES) as usize)
                as *mut vlib_buffer_t;
            to.push_unchecked(BufferRef::from_ptr_mut(ptr));
        }
    }

    /// Enqueues a slice of buffer indices to a next node
    ///
    /// This corresponds to the VPP C function `vlib_buffer_enqueue_to_next`.
    ///
    /// # Safety
    ///
    /// - The length of the from and next slices must match.
    /// - The next node must have a `Vector` type of `u32` (or the C equivalent).
    /// - The next node must have a `Scalar` type of `()` (or the C equivalent).
    /// - The next node must have an `Aux` type of `()` (or the C equivalent).
    /// - `vlib_buffer_func_main` must have been filled in with valid function pointers (which
    ///   will be done by VPP at initialisation time).
    /// - The buffer state, such as `current_data` and `length` must be set according to the
    ///   preconditions of the next node.
    /// - Each entry in the `from` slice must be a valid index to a buffer.
    /// - Each entry in the `nexts` slice must be a valid next node index.
    #[inline(always)]
    pub unsafe fn buffer_enqueue_to_next<N: Node, V: VectorBufferIndex>(
        &self,
        node: &mut NodeRuntimeRef<N>,
        from: &[V],
        nexts: &[u16],
    ) {
        debug_assert_eq!(from.len(), nexts.len());
        // SAFETY: the caller asserts the function preconditions are true
        unsafe {
            (vlib_buffer_func_main
                .buffer_enqueue_to_next_fn
                .unwrap_unchecked())(
                self.as_ptr(),
                node.as_ptr(),
                VectorBufferIndex::as_u32_slice(from).as_ptr().cast_mut(),
                nexts.as_ptr() as *mut u16,
                from.len() as u64,
            )
        }
    }

    /// Allocate a single buffer
    ///
    /// This corresponds to the VPP C API of `vlib_alloc_buffers`.
    #[cfg(feature = "experimental")]
    pub fn alloc_buffer(&self) -> Result<BufferWithContext<'_>, BufferAllocError> {
        // SAFETY: we have a reference to self so the pointer must also be valid, we pass in a
        // buffer pointer that is consistent with the number of buffers asked for, and on exit
        // of the function either the buffer value is filled in with a valid index we have
        // ownership of or not depending on the return value of the function.
        unsafe {
            let mut buffer = 0;
            let res = vlib_helper_buffer_alloc(self.as_ptr(), &mut buffer, 1);
            if res == 1 {
                Ok(BufferWithContext::from_parts(buffer, self))
            } else {
                Err(BufferAllocError)
            }
        }
    }
}
