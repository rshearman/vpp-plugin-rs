//! VPP node abstractions
//!
//! This module contains abstractions around VPP nodes, including node registration,
//! node runtime data, frames, and error counters.

use std::{cell::UnsafeCell, sync::atomic::AtomicU64};

use arrayvec::ArrayVec;
use bitflags::bitflags;

#[cfg(feature = "process-node")]
use crate::vlib::ProcessNode;
use crate::{
    bindings::{
        _vlib_node_registration, VLIB_NODE_FLAG_ADAPTIVE_MODE,
        VLIB_NODE_FLAG_ALLOW_LAZY_NEXT_NODES, VLIB_NODE_FLAG_FRAME_NO_FREE_AFTER_DISPATCH,
        VLIB_NODE_FLAG_IS_DROP, VLIB_NODE_FLAG_IS_HANDOFF, VLIB_NODE_FLAG_IS_OUTPUT,
        VLIB_NODE_FLAG_IS_PUNT, VLIB_NODE_FLAG_SWITCH_FROM_INTERRUPT_TO_POLLING_MODE,
        VLIB_NODE_FLAG_SWITCH_FROM_POLLING_TO_INTERRUPT_MODE, VLIB_NODE_FLAG_TRACE,
        VLIB_NODE_FLAG_TRACE_SUPPORTED, vlib_error_desc_t, vlib_frame_t,
        vlib_helper_get_global_main, vlib_helper_remove_node_from_registrations,
        vlib_node_fn_registration_t, vlib_node_registration_t, vlib_node_runtime_t, vlib_node_t,
    },
    vlib::{MainRef, buffer::BufferRef},
    vppinfra::VecRef,
};

/// Max number of vector elements to process at once per node
///
/// Corresponds to `VLIB_FRAME_SIZE`` in VPP.
pub const FRAME_SIZE: usize = crate::bindings::VLIB_FRAME_SIZE as usize;
/// Frame data alignment
///
/// Corresponds to `VLIB_FRAME_DATA_ALIGN` in VPP.
pub const FRAME_DATA_ALIGN: usize = crate::bindings::VLIB_FRAME_DATA_ALIGN as usize;

/// Runtime data alignment
// Ref: CLIB_ALIGN_MARK (runtime_data_pad, 8) in vlib_node_runtime_t struct definition
pub const RUNTIME_DATA_ALIGN: usize = 8;

/// Trait for defining next nodes of a VPP node
///
/// Typically this trait is implemented automatically using the [`vpp_plugin_macros::NextNodes`]
/// derive macro.
///
/// # Safety
///
/// - The length of C_NAMES must be greater than the maximum value that into_u16 can return
///   (i.e. if implemented for an enum, it should match number of discriminators, and there are no
///   gaps in the discriminator values).
/// - Each pointer in C_NAMES must be a valid, nul-terminated string and must stay valid for the
///   duration of that any nodes using this NextNodes implementation are registered.
pub unsafe trait NextNodes {
    /// Array type for [`Self::C_NAMES`]
    type CNamesArray: AsRef<[*mut ::std::os::raw::c_char]>;
    /// Array of C names of the next nodes indexed by [`Self::into_u16`]
    const C_NAMES: Self::CNamesArray;

    /// The u16 value of this next node
    fn into_u16(self) -> u16;
}

/// Trait for defining error counters of a VPP node
///
/// Typically this trait is implemented automatically using the [`vpp_plugin_macros::ErrorCounters`]
/// derive macro.
///
/// # Safety
///
/// - The length of C_DESCRIPTIONS must be greater than the maximum value that into_u16 can return
///   (i.e. if implemented for an enum, it should match number of discriminators, and there are no
///   gaps in the discriminator values).
/// - Each entry in C_DESCRIPTIONS:
///   - `name` must be a valid nul-terminated string.
///   - `description` must be either null or a valid nul-terminated string.
pub unsafe trait ErrorCounters {
    /// Array type for [`Self::C_DESCRIPTIONS`]
    type CDescriptionsArray: AsRef<[vlib_error_desc_t]>;
    /// Array of C descriptions of the errors indexed by [`Self::into_u16`]
    const C_DESCRIPTIONS: Self::CDescriptionsArray;

    /// The u16 value of this next node
    fn into_u16(self) -> u16;
}

impl<N: Node, const N_NEXT_NODES: usize> NodeRegistration<N, N_NEXT_NODES> {
    /// Creates a new `NodeRegistration` from the given registration data
    pub const fn new(
        registration: _vlib_node_registration<[*mut std::os::raw::c_char; N_NEXT_NODES]>,
    ) -> Self {
        Self {
            registration: UnsafeCell::new(registration),
            _marker: ::std::marker::PhantomData,
        }
    }

    /// Registers the node with VPP
    ///
    /// # Safety
    ///
    /// - Must be called only once for this node registration.
    /// - Must be called from a constructor function that is invoked before VPP initialises.
    /// - The following pointers in the registration data must be valid:
    ///   - `name` (must be a valid, nul-terminated string)
    ///   - `function` (must point to a valid node function)
    ///   - `error_descriptions` (must point to an array of `n_errors` valid `vlib_error_desc_t` entries)
    ///   - `next_nodes` (each entry must be a valid nul-terminated string and length must be at least `n_next_nodes`)
    /// - Other pointers in the registration data must be either valid or null as appropriate.
    /// - `vector_size`, `scalar_size`, and `aux_size` must match the sizes of the corresponding types in `N`.
    /// - `n_errors` must match the discriminants in N::Errors
    /// - `n_next_nodes` must match the discriminants in N::NextNodes
    pub unsafe fn register(&'static self) {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe {
            let vgm = vlib_helper_get_global_main();
            let reg = self.registration.get();
            (*reg).next_registration = (*vgm).node_registrations;
            (*vgm).node_registrations = reg as *mut vlib_node_registration_t;
        }
    }

    /// Unregisters the node from VPP
    ///
    /// # Safety
    ///
    /// - Must be called only once for this node registration.
    /// - Must be called from a destructor function that is invoked after VPP uninitialises.
    /// - The node must have been previously registered with VPP using [`Self::register`].
    pub unsafe fn unregister(&self) {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe {
            let vgm = vlib_helper_get_global_main();
            vlib_helper_remove_node_from_registrations(
                vgm,
                self.registration.get() as *mut vlib_node_registration_t,
            );
        }
    }

    /// Registers a node function with VPP
    ///
    /// # Safety
    /// - The `node_fn` pointer must be valid and point to a properly initialised
    ///   `vlib_node_fn_registration_t`.
    /// - The `node_fn` must not have been previously registered with VPP.
    pub unsafe fn register_node_fn(&self, node_fn: *mut vlib_node_fn_registration_t) {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe {
            let reg = self.registration.get();
            (*node_fn).next_registration = (*reg).node_fn_registrations;
            (*reg).node_fn_registrations = node_fn;
        }
    }

    /// Creates a `&mut NodeRuntimeRef` directly from a pointer
    ///
    /// This is a convenience method that calls [`NodeRuntimeRef::from_ptr_mut`], for code that
    /// has an instance of `NodeRegistration`, but doesn't know the name of the type for the node.
    /// As such, `self` isn't used, it's just taken so that the generic types are known.
    ///
    /// # Safety
    ///
    /// - The same preconditions as [`NodeRuntimeRef::from_ptr_mut`] apply.
    pub unsafe fn node_runtime_from_ptr<'a>(
        &self,
        ptr: *mut vlib_node_runtime_t,
    ) -> &'a mut NodeRuntimeRef<N> {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { NodeRuntimeRef::from_ptr_mut(ptr) }
    }

    /// Creates a `&mut FrameRef` directly from a pointer
    ///
    /// This is a convenience method that calls [`FrameRef::from_ptr_mut`], for code that
    /// has an instance of `NodeRegistration`, but doesn't know the name of the type for the node.
    /// As such, `self` isn't used, it's just taken so that the generic types are known.
    ///
    /// # Safety
    ///
    /// - The same preconditions as [`FrameRef::from_ptr_mut`] apply.
    pub unsafe fn frame_from_ptr<'a>(&self, ptr: *mut vlib_frame_t) -> &'a mut FrameRef<N> {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { FrameRef::from_ptr_mut(ptr) }
    }

    /// Creates a `&mut NodeRef` directly from a pointer
    ///
    /// This is a convenience method that calls [`NodeRef::from_ptr_mut`], for code that
    /// has an instance of `NodeRegistration`, but doesn't know the name of the type for the node.
    /// As such, `self` isn't used, it's just taken so that the generic types are known.
    ///
    /// # Safety
    ///
    /// - The same preconditions as [`NodeRef::from_ptr_mut`] apply.
    pub unsafe fn node_from_ptr<'a>(&self, ptr: *mut vlib_node_t) -> &'a mut NodeRef<N> {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { NodeRef::from_ptr_mut(ptr) }
    }

    /// Returns the name of the node as a pointer to a C string
    ///
    /// Note: no guarantees are made about the validity of the pointer or the string data.
    pub const fn name_ptr(&self) -> *const std::os::raw::c_char {
        // SAFETY: it is safe to access a const pointer to the name as it is not mutated after creation
        unsafe { (*self.registration.get()).name }
    }
}

bitflags! {
    /// Node flags
    #[repr(transparent)]
    #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
    pub struct NodeFlags: u16 {
        /// Don't free the frame after dispatch - processing function keeps the frame
        const FRAME_NO_FREE_AFTER_DISPATCH = VLIB_NODE_FLAG_FRAME_NO_FREE_AFTER_DISPATCH as u16;
        /// Internal node counts as output node for stats purposes
        const IS_OUTPUT = VLIB_NODE_FLAG_IS_OUTPUT as u16;
        /// Internal node counts as drop node for stats purposes
        const IS_DROP = VLIB_NODE_FLAG_IS_DROP as u16;
        /// Internal node counts as punt node for stats purposes
        const IS_PUNT = VLIB_NODE_FLAG_IS_PUNT as u16;
        /// Internal node counts as handoff node for stats purposes
        const IS_HANDOFF = VLIB_NODE_FLAG_IS_HANDOFF as u16;
        /// Current node runtime has traced vectors
        const TRACE = VLIB_NODE_FLAG_TRACE as u16;
        /// The node is in the process of switching from interrupt to polling mode
        const SWITCH_FROM_INTERRUPT_TO_POLLING_MODE = VLIB_NODE_FLAG_SWITCH_FROM_INTERRUPT_TO_POLLING_MODE as u16;
        /// The node is in the process of switching from polling to interrupt mode
        const SWITCH_FROM_POLLING_TO_INTERRUPT_MODE = VLIB_NODE_FLAG_SWITCH_FROM_POLLING_TO_INTERRUPT_MODE as u16;
        /// The node can initiate a per-node packet trace
        const TRACE_SUPPORTED = VLIB_NODE_FLAG_TRACE_SUPPORTED as u16;
        /// The node supports automatic switching between interrupt and polling mode
        const ADAPTIVE_MODE =  VLIB_NODE_FLAG_ADAPTIVE_MODE as u16;
        /// Allows the node registration to refer to a next node that doesn't exist at
        /// registration time
        const ALLOW_LAZY_NEXT_NODES =  VLIB_NODE_FLAG_ALLOW_LAZY_NEXT_NODES as u16;
    }
}

// SAFETY: there is nothing in vlib_node_registration that is tied to a specific thread or that
// mutates global state, so it's safe to send between threads.
unsafe impl<N: Node, const N_NEXT_NODES: usize> Send for NodeRegistration<N, N_NEXT_NODES> {}
// SAFETY: NodeRegistration doesn't allow any modification after creation (and vpp doesn't
// modify it afterwards either), so it's safe to access from multiple threads. The only exception
// to this is the register/unregister/register_node_fn methods, but it's the duty of the caller
// to ensure they are called at times when no other threads have a reference to the object.
unsafe impl<N: Node, const N_NEXT_NODES: usize> Sync for NodeRegistration<N, N_NEXT_NODES> {}

/// Registration information for a VPP node
///
/// Used for registering and unregistering nodes with VPP.
///
/// This is typically created automatically using the [`vpp_plugin_macros::vlib_node`] macro.
pub struct NodeRegistration<N: Node, const N_NEXT_NODES: usize> {
    registration: UnsafeCell<_vlib_node_registration<[*mut std::os::raw::c_char; N_NEXT_NODES]>>,
    _marker: std::marker::PhantomData<N>,
}

/// Reference to a VPP node runtime data
///
/// This is a per-node-instance, per-thread structure containing runtime data about the node.
///
/// A `&mut NodeRuntimeRef` corresponds to `vlib_node_runtime_t *` in C.
#[repr(transparent)]
pub struct NodeRuntimeRef<N: ?Sized>(foreign_types::Opaque, std::marker::PhantomData<N>);

impl<N> NodeRuntimeRef<N> {
    /// Creates a `&mut NodeRuntimeRef` directly from a pointer
    ///
    /// # Safety
    /// - The pointer must be valid and a properly initialised `vlib_node_runtime_t`.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned reference.
    /// - The `runtime_data` field must be set correctly to point to a valid `RuntimeData` instance.
    /// - The `node_index` field must be set correctly to point to a valid node in the VPP node main.
    #[inline(always)]
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_node_runtime_t) -> &'a mut Self {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { &mut *(ptr as *mut _) }
    }

    /// Returns the raw pointer to the underlying `vlib_node_runtime_t`
    #[inline(always)]
    pub fn as_ptr(&self) -> *mut vlib_node_runtime_t {
        self as *const _ as *mut _
    }

    /// Node flags
    #[inline(always)]
    pub fn flags(&self) -> NodeFlags {
        // SAFETY: we have a valid pointer to vlib_node_runtime_t
        unsafe { NodeFlags::from_bits_truncate((*self.as_ptr()).flags) }
    }

    /// Return the node index VPP has assigned this node
    pub fn node_index(&self) -> u32 {
        // SAFETY: we have a valid pointer to vlib_node_runtime_t
        unsafe { (*self.as_ptr()).node_index }
    }

    /// Returns the associated node reference
    pub fn node(&self, vm: &MainRef) -> &NodeRef<N> {
        // SAFETY: we have a valid pointer to vlib_node_runtime_t, and the node_index field is
        // set correctly
        unsafe {
            let nodes = VecRef::from_raw_mut((*vm.as_ptr()).node_main.nodes);
            NodeRef::from_ptr_mut(*nodes.get_unchecked(self.node_index() as usize) as *mut _)
        }
    }
}

impl<N: Node> NodeRuntimeRef<N> {
    /// Returns the node-defined runtime data of the node
    pub fn runtime_data(&self) -> &N::RuntimeData {
        // SAFETY: we have a valid pointer to vlib_node_runtime_t, and the runtime_data field is
        // set correctly
        unsafe { &*((*self.as_ptr()).runtime_data.as_ptr() as *const N::RuntimeData) }
    }

    /// Returns the node-defined runtime data of the node as mutable
    pub fn runtime_data_mut(&mut self) -> &mut N::RuntimeData {
        // SAFETY: we have a valid pointer to vlib_node_runtime_t, and the runtime_data field is
        // set correctly
        unsafe { &mut *((*self.as_ptr()).runtime_data.as_ptr() as *mut N::RuntimeData) }
    }

    /// Increments the given error counter by the specified amount
    ///
    /// See also [`NodeRef::increment_error_counter`].
    pub fn increment_error_counter(&self, vm: &MainRef, counter: N::Errors, increment: u64) {
        self.node(vm)
            .increment_error_counter(vm, counter, increment)
    }
}

#[cfg(feature = "process-node")]
impl<N: ProcessNode> NodeRuntimeRef<N> {
    // Note runtime_data/runtime_data_mut not implemented as they have little benefit for process nodes

    /// Increments the given error counter by the specified amount for process nodes
    ///
    /// See also [`NodeRef::increment_process_error_counter`].
    pub fn increment_process_error_counter(
        &self,
        vm: &MainRef,
        counter: N::Errors,
        increment: u64,
    ) {
        self.node(vm)
            .increment_process_error_counter(vm, counter, increment)
    }
}

/// Reference to a VPP node frame
///
/// A `&mut FrameRef` corresponds to `vlib_frame_t *` in C.
pub struct FrameRef<N: Node + ?Sized>(foreign_types::Opaque, std::marker::PhantomData<N>);

impl<N: Node + ?Sized> FrameRef<N> {
    /// Creates a `&mut FrameRef` directly from a pointer
    ///
    /// # Safety
    /// - The pointer must be valid and a properly initialised `vlib_frame_t`.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    /// - `scalar_offset`, `vector_offset`, and `aux_offset` must be set correctly to point to
    ///   the valid, initialised data areas in the frame, and `n_vectors` must be set correctly
    ///   to indicate the number of valid vector and aux elements.
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_frame_t) -> &'a mut Self {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { &mut *(ptr as *mut _) }
    }

    /// Returns the raw pointer to the underlying `vlib_frame_t`
    pub fn as_ptr(&self) -> *mut vlib_frame_t {
        self as *const _ as *mut _
    }

    /// Returns the vector data elements in the frame as a slice
    pub fn vector(&self) -> &[N::Vector] {
        // SAFETY: the frame's n_vectors field indicates the number of valid vector elements,
        // vector_offset is set correctly when creating the frame, and we have a valid pointer to
        // the frame
        unsafe {
            let vec = (self.as_ptr() as *const u8).add((*(self.as_ptr())).vector_offset as usize)
                as *const N::Vector;
            std::slice::from_raw_parts(vec, (*(self.as_ptr())).n_vectors as usize)
        }
    }

    /// Returns the scalar data in the frame
    pub fn scalar(&self) -> &N::Scalar {
        // SAFETY: scalar_offset is set correctly when creating the frame, and we have a valid
        // pointer to the frame
        unsafe {
            &*((self.as_ptr() as *const u8).add((*(self.as_ptr())).scalar_offset as usize)
                as *const N::Scalar)
        }
    }

    /// Returns the auxiliary data elements in the frame as a slice
    ///
    /// Note: the length of the returned slice is equal to the number of vectors in the frame.
    pub fn aux(&self) -> &[N::Aux] {
        // SAFETY: the frame's n_vectors field indicates the number of valid aux elements,
        // aux_offset is set correctly when creating the frame, and we have a valid pointer to
        // the frame
        unsafe {
            let aux = (self.as_ptr() as *const u8).add((*(self.as_ptr())).aux_offset as usize)
                as *const N::Aux;
            std::slice::from_raw_parts(aux, (*(self.as_ptr())).n_vectors as usize)
        }
    }
}

/// Trait for types that can be used as buffer indices in vector nodes
pub trait VectorBufferIndex: Send + Copy {
    /// Converts a slice of Self to a slice of u32
    fn as_u32_slice(slice: &[Self]) -> &[u32];
}

impl<N, V> FrameRef<N>
where
    N: Node<Vector = V> + ?Sized,
    V: VectorBufferIndex,
{
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
    /// - Each buffer's `current_config_index` must be consistent with the `FeatureData` type. If
    ///   they are not known (i.e. because the caller the node isn't being executed in a feature
    ///   arc), FeatureData should be a zero-sized type such as `()`.
    /// - Must not be called more than once without the framing being flushed in between as
    ///   Rust's reference aliasing rules will be violated.
    #[inline(always)]
    pub unsafe fn get_buffers<'me, 'vm, 'buf: 'vm + 'me, const ARRAY_N: usize>(
        &'me self,
        vm: &'vm MainRef,
        to: &mut ArrayVec<&'buf mut BufferRef<N::FeatureData>, ARRAY_N>,
    ) -> &'me [N::Vector] {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe {
            let from = self.vector();
            vm.get_buffers(N::Vector::as_u32_slice(from), to);
            from
        }
    }
}

/// Reference to a VPP node
///
/// This is a per-node-instance structure containing metadata about the node and certain node
/// state.
///
/// A `&mut NodeRef` corresponds to `vlib_node_t *` in C.
#[repr(transparent)]
pub struct NodeRef<N>(foreign_types::Opaque, std::marker::PhantomData<N>);

impl<N> NodeRef<N> {
    /// Creates a `&mut NodeRef` directly from a pointer
    ///
    /// # Safety
    ///
    /// - The pointer must be valid and a properly initialised `vlib_node_t`.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    /// - The `error_heap_index` field must be set correctly to point to the base index of the
    ///   node's error counters in the VPP error main counters array.
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_node_t) -> &'a mut Self {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { &mut *(ptr as *mut _) }
    }

    /// Returns the raw pointer to the underlying `vlib_node_t`
    pub fn as_ptr(&self) -> *mut vlib_node_t {
        self as *const _ as *mut _
    }
}

impl<N: Node> NodeRef<N> {
    /// Increments the given error counter by the specified amount
    ///
    /// This corresponds to the VPP C function `vlib_node_increment_counter`.
    pub fn increment_error_counter(&self, vm: &MainRef, counter: N::Errors, increment: u64) {
        // SAFETY: we have a valid pointer to vlib_node_t, the error_heap_index field is
        // set correctly, we are the only writer to counters (because it's per-thread),
        // and we perform an atomic store to the counter so that concurrent readers cannot see
        // a partial value.
        unsafe {
            let em = &(*vm.as_ptr()).error_main;
            let node_counter_base_index = (*self.as_ptr()).error_heap_index;
            let ptr = em
                .counters
                .add(node_counter_base_index as usize + counter.into_u16() as usize);
            AtomicU64::from_ptr(ptr).store(*ptr + increment, std::sync::atomic::Ordering::Relaxed);
        }
    }
}

#[cfg(feature = "process-node")]
impl<N: ProcessNode> NodeRef<N> {
    /// Increments the given error counter by the specified amount for process nodes
    ///
    /// This corresponds to the VPP C function `vlib_node_increment_counter`.
    pub fn increment_process_error_counter(
        &self,
        vm: &MainRef,
        counter: N::Errors,
        increment: u64,
    ) {
        // SAFETY: we have a valid pointer to vlib_node_t, the error_heap_index field is
        // set correctly, we are the only writer to counters (because it's per-thread),
        // and we perform an atomic store to the counter so that concurrent readers cannot see
        // a partial value.
        unsafe {
            let em = &(*vm.as_ptr()).error_main;
            let node_counter_base_index = (*self.as_ptr()).error_heap_index;
            let ptr = em
                .counters
                .add(node_counter_base_index as usize + counter.into_u16() as usize);
            AtomicU64::from_ptr(ptr).store(*ptr + increment, std::sync::atomic::Ordering::Relaxed);
        }
    }
}

/// Trait for defining a VPP node
pub trait Node {
    /// Type of vector data sent to the node
    type Vector;
    /// Type of scalar data sent to the node
    ///
    /// The scalar data is shared between all vector elements in a frame.
    ///
    /// This is rarely used and can be set to `()` if not needed.
    type Scalar;
    /// Type of auxiliary data sent to the node
    ///
    /// The auxiliary data is per-vector.
    ///
    /// This is rarely used and can be set to `()` if not needed.
    type Aux;

    /// Type defining the next nodes of this node
    ///
    /// Typically an enum using the [`vpp_plugin_macros::NextNodes`] derive macro.
    type NextNodes: NextNodes;
    /// Type defining the runtime data of this node
    ///
    /// This data is per-node instance and per-thread.
    // Send + Copy due to:
    //     if (vec_len (n->runtime_data) > 0)
    //       clib_memcpy (rt->runtime_data, n->runtime_data,
    //                    vec_len (n->runtime_data));
    //     else
    //       clib_memset (rt->runtime_data, 0, VLIB_NODE_RUNTIME_DATA_SIZE);
    type RuntimeData: Send + Copy;
    /// Type defining the trace data of this node
    // Send due to display from main thread, writing from worker threads.
    // Copy because no destructor will be called on TraceData (since it's discard inside VPP
    // code), and Copy is mutually exclusive with implementing the Drop trait. This isn't a
    // soundness requirement (since Rust doesn't include not leaking memory in its definition
    // of soundless), so this constraint could be dropped be relaxed if it turns out to be too
    // much of a burden.
    type TraceData: Send + Copy;
    /// Type defining the error counters of this node
    ///
    /// Typically an enum using the [`vpp_plugin_macros::ErrorCounters`] derive macro.
    type Errors: ErrorCounters;
    /// Type defining the feature data of this node
    ///
    /// This is available when the node is used as a feature node invoked from a feature arc.
    ///
    /// If the node is not used as a feature node, this type is not used and so can be set to `()`.
    // Send due to setting on main thread and retrieval on worker threads
    type FeatureData: Send;

    /// The packet processing function of the node
    ///
    /// Returns the number of packets processed from the frame.
    ///
    /// # Safety
    /// - The caller must ensure that precondition assumptions for the state of the buffers
    ///   in the frame are met, e.g. that the packets are valid and have the expected headers.
    ///   For example, if the node is expected to be invoked during the ip4-input feature arc, the
    ///   caller must ensure that all packets in the frame are valid IPv4 packets and the current
    ///   data offset is pointing to the start of the IPv4 header. In addition, any assumptions
    ///   about how much of the packet has been linearised must also be upheld.
    /// - The node's precondition assumptions may also inherit from those of next nodes the node
    ///   sends the buffers to.
    unsafe fn function(
        &self,
        vm: &mut MainRef,
        node: &mut NodeRuntimeRef<Self>,
        frame: &mut FrameRef<Self>,
    ) -> u16;
}

#[cfg(test)]
mod tests {
    use crate::{
        bindings::{vlib_error_desc_t, vlib_node_registration_t},
        vlib::{self, node::NodeRegistration},
    };

    enum Errors {}
    // SAFETY: C_DESCRIPTIONS is a valid array that matches number of discriminants in Errors enum
    unsafe impl vlib::node::ErrorCounters for Errors {
        type CDescriptionsArray = [vlib_error_desc_t; 0];

        const C_DESCRIPTIONS: Self::CDescriptionsArray = [];

        fn into_u16(self) -> u16 {
            todo!()
        }
    }

    #[derive(Copy, Clone)]
    enum NextNodes {
        _Drop,
    }

    // SAFETY: C_NAMES is a valid array that matches number of discriminants in NextNodes enum
    unsafe impl vlib::node::NextNodes for NextNodes {
        type CNamesArray = [*mut ::std::os::raw::c_char; 1];

        const C_NAMES: Self::CNamesArray = [c"drop".as_ptr().cast_mut()];

        fn into_u16(self) -> u16 {
            self as u16
        }
    }

    struct Node;
    impl vlib::node::Node for Node {
        type Vector = vlib::BufferIndex;
        type Scalar = ();
        type Aux = ();

        type NextNodes = NextNodes;
        type RuntimeData = ();
        type TraceData = ();
        type Errors = Errors;
        type FeatureData = ();

        unsafe fn function(
            &self,
            _vm: &mut vlib::MainRef,
            _node: &mut vlib::NodeRuntimeRef<Self>,
            _frame: &mut vlib::FrameRef<Self>,
        ) -> u16 {
            unreachable!()
        }
    }

    #[test]
    fn test_node_reg() {
        // NodeRegistration::new is a const function and Rust doesn't generate coverage data
        // when such functions are evaluated at compile time
        // (https://github.com/rust-lang/rust/issues/124732), so force it to be evaluated at
        // runtime.
        let node: NodeRegistration<Node, 0> =
            std::hint::black_box(NodeRegistration::new(vlib_node_registration_t::default()));

        let node = Box::new(node);
        // Get a raw pointer so we can ignore the static lifetime requirement of register
        let node = Box::into_raw(node);

        // SAFETY: preconditions of register/unregister don't have to be met because we are
        // calling this outside of the VPP application, meaning that pointers in the
        // vlib_node_registration_t won't be dereferenced.
        unsafe {
            (*node).register();
            (*node).unregister();

            let _ = Box::from_raw(node);
        }
    }
}
