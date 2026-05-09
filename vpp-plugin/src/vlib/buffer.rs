//! vlib buffer abstraction
//!
//! This module contains abstractions around VPP's `vlib_buffer_t` structure.
//! It provides safe access to buffer fields and operations.
//! It also includes buffer allocation and deallocation functions.
//! The goal is to provide a safe and ergonomic interface for working with VPP buffers.

use std::{fmt, hint::assert_unchecked, mem::ManuallyDrop, mem::MaybeUninit};

use arrayvec::ArrayVec;
use bitflags::bitflags;

use crate::{
    bindings::{
        CLIB_LOG2_CACHE_LINE_BYTES, VLIB_BUFFER_EXT_HDR_VALID, VLIB_BUFFER_IS_TRACED,
        VLIB_BUFFER_MIN_CHAIN_SEG_SIZE, VLIB_BUFFER_NEXT_PRESENT, VLIB_BUFFER_PRE_DATA_SIZE,
        VLIB_BUFFER_TOTAL_LENGTH_VALID, vlib_add_trace, vlib_buffer_func_main, vlib_buffer_t,
        vlib_buffer_t__bindgen_ty_1, vlib_buffer_t__bindgen_ty_1__bindgen_ty_1__bindgen_ty_1,
        vlib_helper_buffer_alloc, vlib_helper_buffer_free,
    },
    vlib::{
        self, MainRef,
        node::{ErrorCounters, Node, NodeRuntimeRef, VectorBufferIndex},
    },
    vppinfra::{
        cache::{prefetch_load, prefetch_store},
        likely,
    },
};

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
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { &*(ptr as *mut _) }
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
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { &mut *(ptr as *mut _) }
    }

    /// Returns the raw pointer to the underlying `vlib_buffer_t`
    #[inline(always)]
    pub fn as_ptr(&self) -> *mut vlib_buffer_t {
        self as *const _ as *mut _
    }

    #[inline(always)]
    fn as_details(&self) -> &vlib_buffer_t__bindgen_ty_1 {
        // SAFETY: since the reference to self is valid, so must be the pointer and it's safe to
        // use the __bindgen_anon_1 union arm since the union is just present to force alignment
        // Creation preconditions mean there are no aliased accesses to the buffer so it's fine
        // to take a reference
        unsafe { (*self.as_ptr()).__bindgen_anon_1.as_ref() }
    }

    #[inline(always)]
    fn as_details_mut(&mut self) -> &mut vlib_buffer_t__bindgen_ty_1 {
        // SAFETY: since the reference to self is valid, so must be the pointer and it's safe to
        // use the __bindgen_anon_1 union arm since the union is just present to force alignment.
        // Creation preconditions mean there are no aliased accesses to the buffer so it's fine
        // to take a reference
        unsafe { (*self.as_ptr()).__bindgen_anon_1.as_mut() }
    }

    #[inline(always)]
    pub(crate) fn as_metadata(&self) -> &vlib_buffer_t__bindgen_ty_1__bindgen_ty_1__bindgen_ty_1 {
        // SAFETY: since the reference to self is valid, so must be the pointer and it's safe to
        // use the __bindgen_anon_1 union arm since the union is just present to force alignment
        // Creation preconditions mean there are no aliased accesses to the buffer so it's fine
        // to take a reference
        unsafe { self.as_details().__bindgen_anon_1.__bindgen_anon_1.as_ref() }
    }

    #[inline(always)]
    pub(crate) fn as_metadata_mut(
        &mut self,
    ) -> &mut vlib_buffer_t__bindgen_ty_1__bindgen_ty_1__bindgen_ty_1 {
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

    /// Get the flags set for this buffer
    #[inline(always)]
    pub fn flags(&self) -> BufferFlags {
        BufferFlags::from_bits_retain(self.as_metadata().flags)
    }

    /// Set the flags for this buffer
    ///
    /// # Safety
    ///
    /// [`BufferFlags::NEXT_PRESENT`] must not be set unless there is a next buffer in the chain.
    /// [`BufferFlags::EXT_HDR_VALID`] must not be set or cleared unless the external buffer manager header is valid
    /// or not valid respectively.
    ///
    #[inline(always)]
    pub unsafe fn set_flags(&mut self, flags: BufferFlags) {
        self.as_metadata_mut().flags = flags.bits()
    }

    /// Get a pointer to the current data
    ///
    /// This corresponds to the VPP C API `vlib_buffer_get_current`.
    ///
    /// # Usage guidance
    ///
    /// Note that the pointer returned may point to uninitialised data depending on the context.
    /// In addition, depending on the context, the remaining data the amount is expected.
    /// Finally, if remaining data is sufficent and it's initialised it may not have been validated
    /// so care must be taken in determining whether or not lengths in the headers can be trusted.
    pub fn current_ptr_mut(&mut self) -> *mut u8 {
        let data = self.data().cast_mut();
        let current_data = self.current_data_offset();

        debug_assert!(current_data >= -(VLIB_BUFFER_PRE_DATA_SIZE as i16));

        // SAFETY: current_data is asserted to be valid and point into valid (but possibly
        // unintialised) data or pre_data.
        unsafe { data.offset(current_data as isize) }
    }

    /// Check if the buffer has space to advance `l` bytes
    ///
    /// This corresponds to the VPP C API `vlib_buffer_has_space`.
    pub fn has_space(&self, l: i16) -> bool {
        self.current_length() >= l as u16
    }

    /// Advance the current data pointer by `l` bytes
    ///
    /// This corresponds to the VPP C API `vlib_buffer_advance`.
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

    /// Append uninitialised data to the end of the current data.
    ///
    /// Returns a pointer to the start of the newly appended uninitialised data.
    ///
    /// This corresponds to the VPP C function `vlib_buffer_put_uninit`.
    ///
    /// # Safety
    ///
    /// The current data plus the space requested must not exceed the data size of the buffer
    /// given during allocation. See [`super::MainRef::buffer_default_data_size`] for buffers
    /// allocated by [`super::MainRef::alloc_buffer`].
    ///
    /// The caller must ensure that the data is correctly initialised before passing the buffer to
    /// code that assumes it is correctly initialised, such as enqueing the buffer another node.
    pub unsafe fn put_uninit(&mut self, size: u16) -> *mut u8 {
        let p = self.tail_mut();
        *self.current_length_mut() += size;
        p
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
            let ptr = data.offset(current_data as isize);
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

    /// Get the total length of the buffer chain not including the first buffer
    #[inline(always)]
    pub fn total_length_not_including_first_buffer(&self) -> u32 {
        debug_assert!(self.flags().contains(BufferFlags::TOTAL_LENGTH_VALID));
        self.as_details().total_length_not_including_first_buffer
    }

    /// Get the total length of the buffer chain from the current offset
    ///
    /// Note that this doesn't take into account any bytes that have been [`Self::advance()`]d
    /// over.
    #[inline(always)]
    pub fn length_in_chain(&self, vm: &vlib::MainRef) -> u64 {
        let len = self.current_length();

        if likely(!self.flags().contains(BufferFlags::NEXT_PRESENT)) {
            return len as u64;
        }

        if likely(self.flags().contains(BufferFlags::TOTAL_LENGTH_VALID)) {
            return len as u64 + self.total_length_not_including_first_buffer() as u64;
        }

        // SAFETY: The buffer pointer is valid and the function is called in a valid context.
        unsafe {
            crate::bindings::vlib_buffer_length_in_chain_slow_path(vm.as_ptr(), self.as_ptr())
        }
    }

    /// Hint to the CPU to prefetch the buffer header for read access.
    ///
    /// This is a performance hint that attempts to bring the buffer header into the CPU cache
    /// prior to reading fields from it. It does not affect program semantics and may be a no-op
    /// on some platforms. Use this when you will shortly read header fields and want to reduce
    /// cache miss latency.
    pub fn prefetch_header_load(&self) {
        prefetch_load(self.as_ptr());
    }

    /// Hint to the CPU to prefetch the buffer header for write access.
    ///
    /// Similar to `prefetch_header_load` but indicates imminent writes to the header. This is a
    /// performance optimization only and does not change observable behaviour other than timing.
    pub fn prefetch_header_store(&self) {
        prefetch_store(self.as_ptr());
    }

    /// Hint to the CPU to prefetch the buffer data area for read access.
    ///
    /// This brings the buffer's data into cache in preparation for reading the packet payload.
    /// It is a non-semantic performance hint and may be ignored on some architectures. Use this
    /// when you will shortly read packet data and want to reduce cache miss latency.
    pub fn prefetch_data_load(&self) {
        prefetch_load(&self.as_details().data);
    }

    /// Hint to the CPU to prefetch the buffer data area for write access.
    ///
    /// Similar to `prefetch_data_load` but indicates the caller will write into the data area.
    /// This is a cache-warming hint to reduce latency on subsequent stores.
    pub fn prefetch_data_store(&self) {
        prefetch_store(&self.as_details().data);
    }
}

/// Owned buffer (with context)
///
/// The `&MainRef` context is necessary to be able to free the buffer on drop.
pub struct BufferWithContext<'a> {
    buffer: u32,
    vm: &'a MainRef,
}

impl<'a> BufferWithContext<'a> {
    /// Creates a `BufferWithContext` directly from a buffer index and a main reference
    ///
    /// # Safety
    /// - The buffer index must be valid and the caller must have ownership of the buffer it
    ///   corresponds to.
    pub unsafe fn from_parts(buffer: BufferIndex, vm: &'a MainRef) -> Self {
        Self {
            buffer: buffer.0,
            vm,
        }
    }

    /// Decomposes a `BufferWithContext` into its component parts
    ///
    /// After calling this the caller is responsible for ensuring the buffer gets freed, either
    /// by calling [`BufferWithContext::from_parts`] or by passing it into another function which
    /// takes ownership of it and eventually causes it to be freed.
    pub fn into_parts(self) -> (BufferIndex, &'a MainRef) {
        let me = ManuallyDrop::new(self);
        (BufferIndex(me.buffer), me.vm)
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
pub struct BufferAllocError;

impl fmt::Display for BufferAllocError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "buffer allocation error")
    }
}

impl std::error::Error for BufferAllocError {}

/// u64 x 8
///
/// This type exists to strongly hint to the compiler that it should emit vector instructions.
///
/// In the future, the implementation might be changed to use standard library portable SIMD once
/// stabilised (https://github.com/rust-lang/rust/issues/86656), or use arch-specific intrinsics
/// (if evidenced by high-enough performance improvement).
#[allow(non_camel_case_types)]
pub(crate) struct u64x8([u64; 8]);

impl u64x8 {
    /// Construct a `u64x8` from an array of 8 `u64`s
    #[inline(always)]
    pub(crate) fn from_array(a: [u64; 8]) -> Self {
        Self(a)
    }

    /// Construct a `u64x8` from a pointer to 8 `u32`s
    #[inline(always)]
    pub(crate) unsafe fn from_u32_ptr(ptr: *const u32) -> Self {
        // SAFETY: The caller must ensure the pointer is valid for reading 8 u32 values.
        unsafe {
            Self([
                *ptr.add(0) as u64,
                *ptr.add(1) as u64,
                *ptr.add(2) as u64,
                *ptr.add(3) as u64,
                *ptr.add(4) as u64,
                *ptr.add(5) as u64,
                *ptr.add(6) as u64,
                *ptr.add(7) as u64,
            ])
        }
    }

    /// Shift each element to the left by a given constant value, assigning the result to `self`
    #[inline(always)]
    pub(crate) fn shift_elements_left<const OFFSET: u32>(&mut self) {
        for a in &mut self.0 {
            *a <<= OFFSET;
        }
    }

    /// Add a given value to each element, returning a new u64x8 with the result
    #[inline(always)]
    pub(crate) fn add_u64(&self, value: u64) -> Self {
        Self::from_array([
            self.0[0] + value,
            self.0[1] + value,
            self.0[2] + value,
            self.0[3] + value,
            self.0[4] + value,
            self.0[5] + value,
            self.0[6] + value,
            self.0[7] + value,
        ])
    }

    /// Write 8 contiguous elements starting from `ptr`
    #[inline(always)]
    pub(crate) unsafe fn store(&self, ptr: *mut u64) {
        // SAFETY: The caller must ensure the pointer is valid for writing 8 u64 values.
        unsafe {
            *ptr.add(0) = self.0[0];
            *ptr.add(1) = self.0[1];
            *ptr.add(2) = self.0[2];
            *ptr.add(3) = self.0[3];
            *ptr.add(4) = self.0[4];
            *ptr.add(5) = self.0[5];
            *ptr.add(6) = self.0[6];
            *ptr.add(7) = self.0[7];
        }
    }
}

/// Round a value up to the next multiple of the given power-of-two
const fn next_multiple_of_pow2(val: usize, pow2: usize) -> usize {
    debug_assert!(pow2.is_power_of_two());
    (val + pow2 - 1) & !(pow2 - 1)
}

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
    /// - The capacity of `from_indices` must be a multiple of 8 (note though that the length is
    ///   allowed not to be). In other words, it must be valid to read multiples of 8 from the
    ///   underlying memory (possibly returning uninitialised or stale data) without faulting.
    #[inline(always)]
    pub unsafe fn get_buffers<'a, 'me, 'buf: 'me, FeatureData, const N: usize>(
        &'me self,
        from_indices: &'a [u32],
        to: &mut ArrayVec<&'buf mut BufferRef<FeatureData>, N>,
    ) {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe {
            debug_assert!(from_indices.len() <= N);
            assert_unchecked(from_indices.len() <= N);

            #[cfg(debug_assertions)]
            for from_index in from_indices {
                let buffer_mem_size = (*(*self.as_ptr()).buffer_main).buffer_mem_size;
                debug_assert!(
                    ((*from_index << CLIB_LOG2_CACHE_LINE_BYTES) as u64) < buffer_mem_size
                );
            }

            let buffer_mem_start = (*(*self.as_ptr()).buffer_main).buffer_mem_start;

            // Check for the ArrayVec capacity being a multiple of 8 and if so the later
            // implementation can perform a write of 8 elements at a time without worrying about
            // writing beyond the end of the ArrayVec. If not, then fall back to a generic
            // implementation. This check will be evaluated at compile time and one implementation
            // or the other chosen.
            if !N.is_multiple_of(8) {
                let base = buffer_mem_start as *const i8;
                for from_index in from_indices.iter() {
                    let ptr = base.add((*from_index << CLIB_LOG2_CACHE_LINE_BYTES) as usize)
                        as *mut vlib_buffer_t;
                    to.push_unchecked(BufferRef::from_ptr_mut(ptr));
                }
                return;
            }

            let mut len = from_indices.len();
            len = next_multiple_of_pow2(len, 8);

            let mut from_index = from_indices.as_ptr();
            let mut to_ptr = to.as_mut_ptr();

            while len >= 64 {
                let mut from_index_x8_1 = u64x8::from_u32_ptr(from_index);
                let mut from_index_x8_2 = u64x8::from_u32_ptr(from_index.add(8));
                let mut from_index_x8_3 = u64x8::from_u32_ptr(from_index.add(2 * 8));
                let mut from_index_x8_4 = u64x8::from_u32_ptr(from_index.add(3 * 8));
                let mut from_index_x8_5 = u64x8::from_u32_ptr(from_index.add(4 * 8));
                let mut from_index_x8_6 = u64x8::from_u32_ptr(from_index.add(5 * 8));
                let mut from_index_x8_7 = u64x8::from_u32_ptr(from_index.add(6 * 8));
                let mut from_index_x8_8 = u64x8::from_u32_ptr(from_index.add(7 * 8));

                from_index_x8_1.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_2.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_3.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_4.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_5.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_6.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_7.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_8.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();

                let buf_ptr_x8_1 = from_index_x8_1.add_u64(buffer_mem_start);
                let buf_ptr_x8_2 = from_index_x8_2.add_u64(buffer_mem_start);
                let buf_ptr_x8_3 = from_index_x8_3.add_u64(buffer_mem_start);
                let buf_ptr_x8_4 = from_index_x8_4.add_u64(buffer_mem_start);
                let buf_ptr_x8_5 = from_index_x8_5.add_u64(buffer_mem_start);
                let buf_ptr_x8_6 = from_index_x8_6.add_u64(buffer_mem_start);
                let buf_ptr_x8_7 = from_index_x8_7.add_u64(buffer_mem_start);
                let buf_ptr_x8_8 = from_index_x8_8.add_u64(buffer_mem_start);

                buf_ptr_x8_1.store(to_ptr as *mut u64);
                buf_ptr_x8_2.store(to_ptr.add(8) as *mut u64);
                buf_ptr_x8_3.store(to_ptr.add(2 * 8) as *mut u64);
                buf_ptr_x8_4.store(to_ptr.add(3 * 8) as *mut u64);
                buf_ptr_x8_5.store(to_ptr.add(4 * 8) as *mut u64);
                buf_ptr_x8_6.store(to_ptr.add(5 * 8) as *mut u64);
                buf_ptr_x8_7.store(to_ptr.add(6 * 8) as *mut u64);
                buf_ptr_x8_8.store(to_ptr.add(7 * 8) as *mut u64);

                to_ptr = to_ptr.add(64);
                from_index = from_index.add(64);
                len -= 64;
            }

            if likely(len >= 32) {
                let mut from_index_x8_1 = u64x8::from_u32_ptr(from_index);
                let mut from_index_x8_2 = u64x8::from_u32_ptr(from_index.add(8));
                let mut from_index_x8_3 = u64x8::from_u32_ptr(from_index.add(2 * 8));
                let mut from_index_x8_4 = u64x8::from_u32_ptr(from_index.add(3 * 8));

                from_index_x8_1.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_2.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_3.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_4.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();

                let buf_ptr_x8_1 = from_index_x8_1.add_u64(buffer_mem_start);
                let buf_ptr_x8_2 = from_index_x8_2.add_u64(buffer_mem_start);
                let buf_ptr_x8_3 = from_index_x8_3.add_u64(buffer_mem_start);
                let buf_ptr_x8_4 = from_index_x8_4.add_u64(buffer_mem_start);

                buf_ptr_x8_1.store(to_ptr as *mut u64);
                buf_ptr_x8_2.store(to_ptr.add(8) as *mut u64);
                buf_ptr_x8_3.store(to_ptr.add(2 * 8) as *mut u64);
                buf_ptr_x8_4.store(to_ptr.add(3 * 8) as *mut u64);

                to_ptr = to_ptr.add(32);
                from_index = from_index.add(32);
                len -= 32;
            }

            if likely(len >= 16) {
                let mut from_index_x8_1 = u64x8::from_u32_ptr(from_index);
                let mut from_index_x8_2 = u64x8::from_u32_ptr(from_index.add(8));

                from_index_x8_1.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                from_index_x8_2.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();

                let buf_ptr_x8_1 = from_index_x8_1.add_u64(buffer_mem_start);
                let buf_ptr_x8_2 = from_index_x8_2.add_u64(buffer_mem_start);

                buf_ptr_x8_1.store(to_ptr as *mut u64);
                buf_ptr_x8_2.store(to_ptr.add(8) as *mut u64);

                to_ptr = to_ptr.add(16);
                from_index = from_index.add(16);
                len -= 16;
            }

            if likely(len > 0) {
                let mut from_index_x8 = u64x8::from_u32_ptr(from_index);
                from_index_x8.shift_elements_left::<CLIB_LOG2_CACHE_LINE_BYTES>();
                let buf_ptr_x8 = from_index_x8.add_u64(buffer_mem_start);
                buf_ptr_x8.store(to_ptr as *mut u64);
            }

            to.set_len(from_indices.len());
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
    pub unsafe fn buffer_enqueue_to_next<N, V: VectorBufferIndex>(
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
    pub fn alloc_buffer(&self) -> Result<BufferWithContext<'_>, BufferAllocError> {
        // SAFETY: we have a reference to self so the pointer must also be valid, we pass in a
        // buffer pointer that is consistent with the number of buffers asked for, and on exit
        // of the function either the buffer value is filled in with a valid index we have
        // ownership of or not depending on the return value of the function.
        unsafe {
            let mut buffer = 0;
            let res = vlib_helper_buffer_alloc(self.as_ptr(), &mut buffer, 1);
            if res == 1 {
                Ok(BufferWithContext::from_parts(buffer.into(), self))
            } else {
                Err(BufferAllocError)
            }
        }
    }

    /// Get the default data size for allocated buffers
    ///
    /// This corresponds to the VPP C API `vlib_buffer_get_default_data_size`.
    pub fn buffer_default_data_size(&self) -> u32 {
        // SAFETY: we have a reference to self so the pointer must also be valid, and MainRef
        // creation preconditions mean that the buffer_main point must also be valid.
        unsafe { (*(*self.as_ptr()).buffer_main).default_data_size }
    }
}

#[cfg(test)]
mod tests {
    use arrayvec::ArrayVec;

    use crate::{
        bindings::{CLIB_LOG2_CACHE_LINE_BYTES, vlib_buffer_main_t, vlib_buffer_t, vlib_main_t},
        vlib::{MainRef, node::FRAME_SIZE},
    };

    #[test]
    fn get_buffers() {
        let buffer = vlib_buffer_t::default();
        // This is picked deliberately to not be 128 - 8 - 1 to be the worst case in terms of maximising the code
        // paths that need to be taken
        const BUFFERS_N: usize = 119;
        let buffers = [buffer; BUFFERS_N];
        let buffer_indices: ArrayVec<u32, 128> = (0..buffers.len() as u32)
            .map(|n| {
                n * (std::mem::size_of::<vlib_buffer_t>() as u32 >> CLIB_LOG2_CACHE_LINE_BYTES)
            })
            .collect();
        let mut buffer_main = vlib_buffer_main_t {
            buffer_mem_start: std::ptr::addr_of!(buffers) as u64,
            buffer_mem_size: std::mem::size_of_val(&buffers) as u64,
            ..vlib_buffer_main_t::default()
        };
        let mut main = vlib_main_t {
            buffer_main: std::ptr::addr_of_mut!(buffer_main),
            ..vlib_main_t::default()
        };
        // SAFETY: pointers used by MainRef::get_buffers are initialised correctly and valid for
        // the duration of the call.
        unsafe {
            let mut to = ArrayVec::new();
            let main_ref = MainRef::from_ptr_mut(std::ptr::addr_of_mut!(main));
            main_ref.get_buffers::<(), FRAME_SIZE>(&buffer_indices, &mut to);
            assert_eq!(to.len(), BUFFERS_N);
            for (i, buf_ref) in to.iter().enumerate() {
                assert!(
                    buf_ref.as_ptr().cast_const() == std::ptr::addr_of!(buffers[i]),
                    "Buffer index {i} pointers don't match: {:p} expected {:p}",
                    buf_ref.as_ptr(),
                    std::ptr::addr_of!(buffers[i])
                );
            }
        }
    }
}
