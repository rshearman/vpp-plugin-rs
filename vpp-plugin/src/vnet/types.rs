//! VPP networking types

use std::{ffi::CStr, fmt};

use crate::{
    bindings::{
        ip4_header_t, ip6_address_t, ip6_header_t, vlib_helper_format_ip4_header,
        vlib_helper_format_ip6_header,
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
