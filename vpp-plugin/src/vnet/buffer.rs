#![allow(missing_docs)]

//! VNET buffer flags

use bitflags::bitflags;

use crate::bindings::{
    VNET_BUFFER_F_AVAIL1, VNET_BUFFER_F_AVAIL2, VNET_BUFFER_F_AVAIL3, VNET_BUFFER_F_AVAIL4,
    VNET_BUFFER_F_AVAIL5, VNET_BUFFER_F_AVAIL6, VNET_BUFFER_F_AVAIL7, VNET_BUFFER_F_AVAIL8,
    VNET_BUFFER_F_AVAIL9, VNET_BUFFER_F_FLOW_REPORT, VNET_BUFFER_F_GSO, VNET_BUFFER_F_IS_DVR,
    VNET_BUFFER_F_IS_IP4, VNET_BUFFER_F_IS_IP6, VNET_BUFFER_F_IS_NATED,
    VNET_BUFFER_F_L2_HDR_OFFSET_VALID, VNET_BUFFER_F_L3_HDR_OFFSET_VALID,
    VNET_BUFFER_F_L4_CHECKSUM_COMPUTED, VNET_BUFFER_F_L4_CHECKSUM_CORRECT,
    VNET_BUFFER_F_L4_HDR_OFFSET_VALID, VNET_BUFFER_F_LOCALLY_ORIGINATED,
    VNET_BUFFER_F_LOOP_COUNTER_VALID, VNET_BUFFER_F_OFFLOAD, VNET_BUFFER_F_QOS_DATA_VALID,
    VNET_BUFFER_F_SPAN_CLONE, VNET_BUFFER_F_VLAN_1_DEEP, VNET_BUFFER_F_VLAN_2_DEEP,
};

bitflags! {
    /// VNET buffer flags
    pub struct BufferFlags: u32 {
        #[allow(missing_docs)]
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

impl crate::vlib::buffer::BufferFlags {
    /// Get the VNET buffer flags from the VLIB buffer flags
    pub fn vnet_flags(&self) -> BufferFlags {
        BufferFlags::from_bits_retain(self.bits())
    }
}
