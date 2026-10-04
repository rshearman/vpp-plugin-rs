#![allow(missing_docs)]

//! VNET buffer flags

use bitflags::bitflags;

#[cfg(not(vpp_shared_feature_config_heap))]
use crate::bindings::vnet_config_main_t;
use crate::{
    bindings::{
        VNET_BUFFER_F_AVAIL1, VNET_BUFFER_F_AVAIL2, VNET_BUFFER_F_AVAIL3, VNET_BUFFER_F_AVAIL4,
        VNET_BUFFER_F_AVAIL5, VNET_BUFFER_F_AVAIL6, VNET_BUFFER_F_AVAIL7, VNET_BUFFER_F_AVAIL8,
        VNET_BUFFER_F_AVAIL9, VNET_BUFFER_F_FLOW_REPORT, VNET_BUFFER_F_GSO, VNET_BUFFER_F_IS_DVR,
        VNET_BUFFER_F_IS_IP4, VNET_BUFFER_F_IS_IP6, VNET_BUFFER_F_IS_NATED,
        VNET_BUFFER_F_L2_HDR_OFFSET_VALID, VNET_BUFFER_F_L3_HDR_OFFSET_VALID,
        VNET_BUFFER_F_L4_CHECKSUM_COMPUTED, VNET_BUFFER_F_L4_CHECKSUM_CORRECT,
        VNET_BUFFER_F_L4_HDR_OFFSET_VALID, VNET_BUFFER_F_LOCALLY_ORIGINATED,
        VNET_BUFFER_F_LOOP_COUNTER_VALID, VNET_BUFFER_F_OFFLOAD, VNET_BUFFER_F_QOS_DATA_VALID,
        VNET_BUFFER_F_SPAN_CLONE, VNET_BUFFER_F_VLAN_1_DEEP, VNET_BUFFER_F_VLAN_2_DEEP,
        feature_main, vlib_rx_or_tx_t_VLIB_RX, vlib_rx_or_tx_t_VLIB_TX, vnet_buffer_opaque_t,
    },
    vnet::types::SwIfIndex,
};

bitflags! {
    /// VNET buffer flags
    pub struct BufferFlags: u32 {
        const L4_CHECKSUM_COMPUTED = VNET_BUFFER_F_L4_CHECKSUM_COMPUTED as u32;
        const L4_CHECKSUM_CORRECT = VNET_BUFFER_F_L4_CHECKSUM_CORRECT as u32;
        const VLAN_2_DEEP = VNET_BUFFER_F_VLAN_2_DEEP as u32;
        const VLAN_1_DEEP = VNET_BUFFER_F_VLAN_1_DEEP as u32;
        const SPAN_CLONE = VNET_BUFFER_F_SPAN_CLONE as u32;
        const LOOP_COUNTER_VALID = VNET_BUFFER_F_LOOP_COUNTER_VALID as u32;
        const LOCALLY_ORIGINATED = VNET_BUFFER_F_LOCALLY_ORIGINATED as u32;
        const IS_IP4 = VNET_BUFFER_F_IS_IP4 as u32;
        const IS_IP6 = VNET_BUFFER_F_IS_IP6 as u32;
        const OFFLOAD = VNET_BUFFER_F_OFFLOAD as u32;
        const IS_NATED = VNET_BUFFER_F_IS_NATED as u32;
        const L2_HDR_OFFSET_VALID = VNET_BUFFER_F_L2_HDR_OFFSET_VALID as u32;
        const L3_HDR_OFFSET_VALID = VNET_BUFFER_F_L3_HDR_OFFSET_VALID as u32;
        const L4_HDR_OFFSET_VALID = VNET_BUFFER_F_L4_HDR_OFFSET_VALID as u32;
        const FLOW_REPORT = VNET_BUFFER_F_FLOW_REPORT as u32;
        const IS_DVR = VNET_BUFFER_F_IS_DVR as u32;
        const QOS_DATA_VALID = VNET_BUFFER_F_QOS_DATA_VALID as u32;
        const GSO = VNET_BUFFER_F_GSO as u32;
        const AVAIL1 = VNET_BUFFER_F_AVAIL1 as u32;
        const AVAIL2 = VNET_BUFFER_F_AVAIL2 as u32;
        const AVAIL3 = VNET_BUFFER_F_AVAIL3 as u32;
        const AVAIL4 = VNET_BUFFER_F_AVAIL4 as u32;
        const AVAIL5 = VNET_BUFFER_F_AVAIL5 as u32;
        const AVAIL6 = VNET_BUFFER_F_AVAIL6 as u32;
        const AVAIL7 = VNET_BUFFER_F_AVAIL7 as u32;
        const AVAIL8 = VNET_BUFFER_F_AVAIL8 as u32;
        const AVAIL9 = VNET_BUFFER_F_AVAIL9 as u32;

        // vlib flags not represented here
        const _ = !0;
    }
}

impl BufferFlags {
    /// Get the VLIB buffer flags from the VNET buffer flags
    pub fn as_vlib_flags(&self) -> crate::vlib::buffer::BufferFlags {
        crate::vlib::buffer::BufferFlags::from_bits_retain(self.bits())
    }
}

impl crate::vlib::buffer::BufferFlags {
    /// Get the VNET buffer flags from the VLIB buffer flags
    pub fn vnet_flags(&self) -> BufferFlags {
        BufferFlags::from_bits_retain(self.bits())
    }
}

/// Reference to a VNET buffer
///
/// A `&mut BufferRef` is equivalent to a `vnet_buffer_opaque_t *` in C (a `*vnet_buffer_opaque_t`
/// in Rust).
#[repr(transparent)]
pub struct BufferRef(foreign_types::Opaque);

impl BufferRef {
    /// Create a `&BufferRef` from a raw pointer
    ///
    /// # Safety
    ///
    /// - The pointer must be a valid and properly initialised `vlib_buffer_t`.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    #[inline(always)]
    pub unsafe fn from_ptr<'a>(ptr: *const vnet_buffer_opaque_t) -> &'a BufferRef {
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
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vnet_buffer_opaque_t) -> &'a mut BufferRef {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { &mut *(ptr as *mut _) }
    }

    /// Returns the raw pointer to the underlying `vnet_buffer_opaque_t`
    #[inline(always)]
    pub fn as_ptr(&self) -> *mut vnet_buffer_opaque_t {
        self as *const _ as *mut _
    }

    /// Returns the index of the feature arc that the buffer is being processed from
    ///
    /// Absent on VPP builds with a shared feature config heap (26.06+), which no longer record
    /// the arc per buffer.
    #[cfg(not(vpp_shared_feature_config_heap))]
    #[inline(always)]
    pub fn feature_arc_index(&self) -> u8 {
        // SAFETY: since the reference to self is valid, so must be the pointer
        unsafe { (*self.as_ptr()).feature_arc_index }
    }

    /// Returns the index of the receive software interface
    pub fn rx_sw_if_index(&self) -> SwIfIndex {
        // SAFETY: since the reference to self is valid, so must be the pointer
        SwIfIndex::new(unsafe { (*self.as_ptr()).sw_if_index[vlib_rx_or_tx_t_VLIB_RX as usize] })
    }

    /// Set the index of the receive software interface
    pub fn set_rx_sw_if_index(&mut self, sw_if_index: SwIfIndex) {
        // SAFETY: since the reference to self is valid, so must be the pointer
        unsafe {
            (*self.as_ptr()).sw_if_index[vlib_rx_or_tx_t_VLIB_TX as usize] = sw_if_index.into()
        };
    }

    /// Returns the index of the transmit software interface
    pub fn tx_sw_if_index(&self) -> Option<SwIfIndex> {
        // SAFETY: since the reference to self is valid, so must be the pointer
        let sw_if_index = unsafe { (*self.as_ptr()).sw_if_index[vlib_rx_or_tx_t_VLIB_TX as usize] };
        if sw_if_index == u32::MAX {
            None
        } else {
            Some(sw_if_index.into())
        }
    }

    /// Set the index of the transmit software interface
    ///
    /// If `sw_if_index` is `None` then it will be set to [`u32::MAX`], which indicates to
    /// various nodes to use the interface from forwarding lookups.
    pub fn set_tx_sw_if_index(&mut self, sw_if_index: Option<SwIfIndex>) {
        let sw_if_index = sw_if_index.map(u32::from).unwrap_or(u32::MAX);
        // SAFETY: since the reference to self is valid, so must be the pointer
        unsafe { (*self.as_ptr()).sw_if_index[vlib_rx_or_tx_t_VLIB_TX as usize] = sw_if_index };
    }
}

impl<FeatureData> crate::vlib::BufferRef<FeatureData> {
    pub fn vnet_buffer(&self) -> &BufferRef {
        let ptr = &self.as_metadata().opaque as *const _;
        // SAFETY: ptr is valid since reference to self is valid, and the representation is a
        // valid `vnet_buffer_opaque_t`
        unsafe { BufferRef::from_ptr(ptr as *const vnet_buffer_opaque_t) }
    }

    pub fn vnet_buffer_mut(&mut self) -> &mut BufferRef {
        let ptr = &mut self.as_metadata_mut().opaque as *mut _;
        // SAFETY: ptr is valid since reference to self is valid, and the representation is a
        // valid `vnet_buffer_opaque_t`
        unsafe { BufferRef::from_ptr_mut(ptr as *mut vnet_buffer_opaque_t) }
    }
}

/// Returns VNET config data for the given config index
///
/// In the process, the config index is advanced to the next one.
///
/// # Safety
///
/// - `cm` must be a valid pointer to a `vnet_config_main_t` structure.
/// - `(*cm).config_string_heap` must be a valid pointer to a contiguous heap of `u32` values.
/// - `*config_index` must be a valid index into the heap: `index + (size_of::<FeatureData>() /
///   size_of::<u32>()) + 1 <= heap_length` must be satisfied, where `heap_length` is the
///   number of `u32` elements in the heap. Additionally, the memory at
///   `(*cm).config_string_heap.add(index)` must be properly aligned for `FeatureData` and
///   contain valid data of that type, followed by a valid `u32` next-index value.
/// - `FeatureData` must exactly match the type that was stored at this config index during
///   configuration creation, including any alignment and representation requirements.
#[cfg(not(vpp_shared_feature_config_heap))]
#[inline(always)]
unsafe fn vnet_get_config_data<FeatureData: Copy>(
    cm: *const vnet_config_main_t,
    config_index: &mut u32,
) -> (u32, FeatureData) {
    // SAFETY: forwarded verbatim; `cm`'s heap satisfies `config_data_at`'s preconditions per
    // this function's.
    unsafe { config_data_at((*cm).config_string_heap, config_index) }
}

/// Returns config data at `*config_index` in a raw config string heap, advancing the index.
///
/// The shared-heap form of [`vnet_get_config_data`]: `vnet_get_config_shared_data` in VPP's
/// `vnet/config.h`.
///
/// # Safety
///
/// Same as [`vnet_get_config_data`], with `heap` in place of `(*cm).config_string_heap`.
#[inline(always)]
unsafe fn config_data_at<FeatureData: Copy>(
    heap: *const u32,
    config_index: &mut u32,
) -> (u32, FeatureData) {
    // SAFETY: function preconditions mean that this pointer arithmetic is valid and matches what
    // VPP expects
    unsafe {
        let index = *config_index;

        let d = heap.add(index as usize);

        let n = std::mem::size_of::<FeatureData>().div_ceil(std::mem::size_of_val(&*d));

        // The last u32 is the next index
        let next = *d.add(n);

        // Advance config index to next config
        *config_index = index + n as u32 + 1;

        (next, *(d as *const FeatureData))
    }
}

impl<FeatureData: Copy> crate::vlib::BufferRef<FeatureData> {
    /// Get the next feature node and feature data for this buffer
    ///
    /// Used when continuing to the next feature node in a node invoked from a feature arc.
    ///
    /// # Safety
    ///
    /// Must only be used from nodes when invoked from a feature arc.
    #[cfg(vpp_shared_feature_config_heap)]
    #[inline(always)]
    pub unsafe fn vnet_feature_next(&mut self) -> (u32, FeatureData) {
        // SAFETY: method precondition means `current_config_index` is a valid index into the
        // shared feature config heap. VPP only rebuilds that heap with the worker barrier held,
        // so no worker can observe it mid-update.
        unsafe {
            config_data_at(
                feature_main.shared_feature_config_heap,
                &mut self.as_metadata_mut().__bindgen_anon_1.current_config_index,
            )
        }
    }

    /// Get the next feature node and feature data for this buffer
    ///
    /// Used when continuing to the next feature node in a node invoked from a feature arc.
    ///
    /// # Safety
    ///
    /// Must only be used from nodes when invoked from a feature arc.
    #[cfg(not(vpp_shared_feature_config_heap))]
    #[inline(always)]
    pub unsafe fn vnet_feature_next(&mut self) -> (u32, FeatureData) {
        let arc = self.vnet_buffer().feature_arc_index();
        // SAFETY: method precondition means that arc is a valid index into
        // `feature_main.feature_config_mains`, and then also that the `current_config_index`
        // buffer field is a valid config index for that feature arc.
        // Access to `feature_main.feature_config_mains` is safe without locking because VPP only
        // modifies this during init, before any buffers are allocated.
        unsafe {
            let cm = *feature_main.feature_config_mains.add(arc as usize);

            vnet_get_config_data(
                &cm.config_main,
                &mut self.as_metadata_mut().__bindgen_anon_1.current_config_index,
            )
        }
    }

    /// The buffer's feature config index: its position in its arc's feature config string (in
    /// the heap shared by every arc, from VPP 26.06).
    ///
    /// Two buffers with equal indices (and, before 26.06, on the same arc) are at the same point
    /// of the same config string, so [`Self::vnet_feature_next`] yields the same next node and the same advanced
    /// index for both.
    #[inline(always)]
    pub fn current_config_index(&self) -> u32 {
        // SAFETY: plain u32 read of an initialised metadata field of a valid buffer
        unsafe { self.as_metadata().__bindgen_anon_1.current_config_index }
    }

    /// Sets the buffer's feature config index; see [`Self::current_config_index`].
    #[inline(always)]
    pub fn set_current_config_index(&mut self, index: u32) {
        self.as_metadata_mut().__bindgen_anon_1.current_config_index = index;
    }
}
