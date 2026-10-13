//! VPP networking types

use std::fmt;

use crate::{
    bindings::{
        ip4_header_t, ip6_address_t, ip6_header_t, vlib_helper_format_ip4_header,
        vlib_helper_format_ip6_header, vlib_helper_format_vnet_sw_if_index_name,
        vlib_helper_unformat_vnet_sw_interface, vnet_get_main,
    },
    vlib, vppinfra,
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

impl fmt::Debug for ip6_header_t {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Display for ip6_header_t {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: vlib_helper_format_ip6_header returns a valid Vec of (not NUL-terminated)
        // string bytes (on allocation failure it aborts)
        let s = unsafe {
            vppinfra::vec::Vec::from_raw(vlib_helper_format_ip6_header(
                std::ptr::null_mut(),
                self,
                std::mem::size_of::<Self>(),
            ))
        };
        write!(f, "{}", String::from_utf8_lossy(&s))
    }
}

impl Clone for ip6_header_t {
    fn clone(&self) -> Self {
        Self { ..*self }
    }
}

/// Error for [`SwIfIndex::parse`]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct SwIfIndexParseError;

impl fmt::Display for SwIfIndexParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Software interface index parse error")
    }
}

impl std::error::Error for SwIfIndexParseError {}

/// Helper struct for displaying [`SwIfIndex`]
pub struct SwIfIndexDisplay<'a> {
    sw_if_index: SwIfIndex,
    _vm: &'a vlib::MainRef,
}

impl<'a> fmt::Display for SwIfIndexDisplay<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: the variable argument list matches what the format_vnet_sw_if_index_name
        // implementation in VPP expects, and it's allowed for the s parameter to be null as
        // this is a VPP vector which automatically allocates on null. The function cannot fail
        // so the return value is always a valid vector pointer (of string bytes that aren't
        // NUL-terminated). We are on a VPP main/worker thread so the shared state is initialised
        // and there cannot be any data races in accessing it (since the state must only be
        // mutated with the barrier taken).
        let s = unsafe {
            vppinfra::vec::Vec::from_raw(vlib_helper_format_vnet_sw_if_index_name(
                std::ptr::null_mut(),
                vnet_get_main(),
                self.sw_if_index.0,
            ))
        };
        write!(f, "{}", String::from_utf8_lossy(&s))
    }
}

/// Software interface index
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SwIfIndex(u32);

impl SwIfIndex {
    /// The local0 interface
    pub const LOCAL0: Self = SwIfIndex::new(0);

    /// Creates a software interface index from a `u32` value
    pub const fn new(sw_if_index: u32) -> Self {
        Self(sw_if_index)
    }

    /// Parse the given interface name to a value of this type
    pub fn parse(_vm: &vlib::MainRef, s: &str) -> Result<Self, SwIfIndexParseError> {
        let mut i = vppinfra::unformat::UnformatInput::from(s);
        let mut sw_if_index = 0;
        // SAFETY: the variable argument list matches what the unformat_vnet_sw_interface
        // implementation in VPP expects, and we are on a VPP main/worker thread so the shared
        // state is initialised and there cannot be any data races in accessing it (since the
        // state must only be mutated with the barrier taken).
        let ret = unsafe {
            vlib_helper_unformat_vnet_sw_interface(i.as_ptr(), vnet_get_main(), &mut sw_if_index)
        };
        if ret > 0 {
            Ok(Self(sw_if_index))
        } else {
            Err(SwIfIndexParseError)
        }
    }

    /// Return an object which implements [`fmt::Display`]
    #[must_use]
    pub fn display<'a>(&self, vm: &'a vlib::MainRef) -> SwIfIndexDisplay<'a> {
        SwIfIndexDisplay {
            sw_if_index: *self,
            _vm: vm,
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

impl fmt::Debug for ip4_header_t {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <Self as fmt::Display>::fmt(self, f)
    }
}

impl fmt::Display for ip4_header_t {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // SAFETY: vlib_helper_format_ip4_header returns a valid Vec of (not NUL-terminated)
        // string bytes (on allocation failure it aborts)
        let s = unsafe {
            vppinfra::vec::Vec::from_raw(vlib_helper_format_ip4_header(
                std::ptr::null_mut(),
                self,
                std::mem::size_of::<Self>(),
            ))
        };
        write!(f, "{}", String::from_utf8_lossy(&s))
    }
}
