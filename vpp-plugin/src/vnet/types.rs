//! VPP networking types

use std::{ffi::CStr, fmt, str::FromStr};

use crate::{
    bindings::{
        ip4_header_t, ip6_address_t, ip6_header_t, vlib_helper_format_ip4_header,
        vlib_helper_format_ip6_header, vlib_helper_format_vnet_sw_if_index_name,
        vlib_helper_unformat_vnet_sw_interface, vnet_get_main,
    },
    vppinfra,
};

impl ip6_address_t {
    /// Creates a new IPv6 address with all bytes set to zero
    pub const fn new() -> Self {
        Self { as_u8: [0; 16] }
    }
}

impl ip6_header_t {
    /// Creates a new IPv6 header with all fields set to zero
    pub const fn new() -> Self {
        Self {
            ip_version_traffic_class_and_flow_label: 0,
            payload_length: 0,
            protocol: 0,
            hop_limit: 0,
            src_address: ip6_address_t::new(),
            dst_address: ip6_address_t::new(),
        }
    }
}

impl fmt::Display for ip6_header_t {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: vlib_helper_format_ip4_header returns a valid Vec representing a C string (on
        // allocation failure it aborts)
        unsafe {
            let s = vppinfra::vec::Vec::from_raw(vlib_helper_format_ip6_header(
                std::ptr::null_mut(),
                self,
                std::mem::size_of::<Self>(),
            ));
            let cstr = CStr::from_bytes_with_nul_unchecked(&s);
            write!(f, "{}", cstr.to_string_lossy())
        }
    }
}

impl Clone for ip6_header_t {
    fn clone(&self) -> Self {
        // SAFETY: if self is a valid reference then it must also be a valid pointer for reading
        // its fields, and reading unaligned from a valid pointer is always safe
        unsafe {
            Self {
                ip_version_traffic_class_and_flow_label: std::ptr::read_unaligned(
                    std::ptr::addr_of!(self.ip_version_traffic_class_and_flow_label),
                ),
                payload_length: std::ptr::read_unaligned(std::ptr::addr_of!(self.payload_length)),
                protocol: self.protocol,
                hop_limit: self.hop_limit,
                src_address: self.src_address,
                dst_address: self.dst_address,
            }
        }
    }
}

/// Software interface index
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SwIfIndex(u32);

impl SwIfIndex {
    /// Creates a software interface index from a `u32` value
    pub const fn new(sw_if_index: u32) -> Self {
        Self(sw_if_index)
    }
}

impl fmt::Display for SwIfIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: the variable argument list matches what the format_vnet_sw_if_index_name
        // implementation in VPP expects, and it's allowed for the s parameter to be null as
        // this is a VPP vector which automatically allocates on null. The function cannot fail
        // so the return value is always a valid vector pointer.
        unsafe {
            let s = vppinfra::vec::Vec::from_raw(vlib_helper_format_vnet_sw_if_index_name(
                std::ptr::null_mut(),
                vnet_get_main(),
                self.0,
            ));
            let cstr = CStr::from_bytes_with_nul_unchecked(&s);
            write!(f, "{}", cstr.to_string_lossy())
        }
    }
}

impl From<u32> for SwIfIndex {
    fn from(value: u32) -> Self {
        Self(value)
    }
}

impl From<SwIfIndex> for u32 {
    fn from(value: SwIfIndex) -> Self {
        value.0
    }
}

impl FromStr for SwIfIndex {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut i = vppinfra::unformat::UnformatInput::from(s);
        let mut sw_if_index = 0;
        // SAFETY: the variable argument list matches what the unformat_vnet_sw_interface
        // implementation in VPP expects.
        let ret = unsafe {
            vlib_helper_unformat_vnet_sw_interface(i.as_ptr(), vnet_get_main(), &mut sw_if_index)
        };
        if ret > 0 {
            Ok(Self(sw_if_index))
        } else {
            Err(())
        }
    }
}

impl fmt::Display for ip4_header_t {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: vlib_helper_format_ip4_header returns a valid Vec representing a C string (on
        // allocation failure it aborts)
        unsafe {
            let s = vppinfra::vec::Vec::from_raw(vlib_helper_format_ip4_header(
                std::ptr::null_mut(),
                self,
                std::mem::size_of::<Self>(),
            ));
            let cstr = CStr::from_bytes_with_nul_unchecked(&s);
            write!(f, "{}", cstr.to_string_lossy())
        }
    }
}
