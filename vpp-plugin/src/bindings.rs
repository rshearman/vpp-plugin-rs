// Allow all sorts of warnings triggered by bindgen-generated code
#![allow(
    clippy::all,
    clippy::undocumented_unsafe_blocks,
    missing_docs,
    non_camel_case_types,
    non_upper_case_globals,
    non_snake_case,
    improper_ctypes,
    unreachable_pub,
    unsafe_op_in_unsafe_fn,
    missing_copy_implementations
)]

//! Bindings to VPP's C API
//!
//! These are low-level bindings to VPP's C API and are primarily intended for internal use by
//! higher-level abstractions in this crate. Direct use of these bindings is generally discouraged
//! unless absolutely necessary, as they are unsafe and do not provide any safety guarantees.
//!
//! This isn't intended to be a complete set of bindings to all of VPP's C API, only the parts that are
//! necessary for implementing the higher-level abstractions in this crate. They should be
//! considered an unstable part of the API and, as such, bindings may be removed over time without
//! incrementing the major version number or prior deprecation.
//!
//! Note that these bindings may vary between VPP versions and targets, and care should be taken
//! to ensure compatibility when using them directly.

include!(concat!(env!("OUT_DIR"), "/vlib_bindings.rs"));

// bindgen generates duplicate definitions for these types due to forward declarations in vlib/trace.h, so we blocklist them and declare them manually here

#[repr(C)]
#[derive(Debug, Copy, Clone)]
pub struct vlib_trace_main_t {
    pub trace_buffer_pool: *mut *mut vlib_trace_header_t,
    pub last_main_loop_count: u32_,
    pub filter_node_index: u32_,
    pub filter_flag: u32_,
    pub filter_count: u32_,
    pub trace_enable: u32_,
    pub nodes: *mut vlib_trace_node_t,
    pub verbose: ::std::os::raw::c_int,
    pub trace_buffer_callback: vlib_trace_buffer_callback_t,
    pub add_trace_callback: vlib_add_trace_callback_t,
    pub current_trace_filter_function: vlib_is_packet_traced_fn_t,
}

#[repr(C)]
#[repr(align(64))]
#[derive(Copy, Clone)]
pub struct vlib_buffer_t {
    pub cacheline0: __BindgenUnionField<[u8_; 0usize]>,
    pub __bindgen_anon_1: __BindgenUnionField<vlib_buffer_t__bindgen_ty_1>,
    pub bindgen_union_field: [u8; 256usize],
}

// bindgen generates duplicate definitions for these types due to forward declarations in vnet/interface.h, so we blocklist them and declare them manually here

#[repr(C)]
#[derive(Copy, Clone)]
pub struct vnet_sw_interface_t {
    pub _bitfield_align_1: [u16; 0],
    pub _bitfield_1: __BindgenBitfieldUnit<[u8; 2usize]>,
    pub flags: vnet_sw_interface_flags_t,
    pub sw_if_index: u32_,
    pub sup_sw_if_index: u32_,
    pub unnumbered_sw_if_index: u32_,
    pub hw_if_index: u32_,
    pub mtu: [u32_; 4usize],
    pub sub: vnet_sub_interface_t,
    pub p2p: vnet_p2p_sub_interface_t,
    pub flood_class: vnet_flood_class_t,
}
#[allow(clippy::unnecessary_operation, clippy::identity_op)]
const _: () = {
    ["Size of vnet_sw_interface_t"][::std::mem::size_of::<vnet_sw_interface_t>() - 68usize];
    ["Alignment of vnet_sw_interface_t"][::std::mem::align_of::<vnet_sw_interface_t>() - 4usize];
    ["Offset of field: vnet_sw_interface_t::flags"]
        [::std::mem::offset_of!(vnet_sw_interface_t, flags) - 2usize];
    ["Offset of field: vnet_sw_interface_t::sw_if_index"]
        [::std::mem::offset_of!(vnet_sw_interface_t, sw_if_index) - 4usize];
    ["Offset of field: vnet_sw_interface_t::sup_sw_if_index"]
        [::std::mem::offset_of!(vnet_sw_interface_t, sup_sw_if_index) - 8usize];
    ["Offset of field: vnet_sw_interface_t::unnumbered_sw_if_index"]
        [::std::mem::offset_of!(vnet_sw_interface_t, unnumbered_sw_if_index) - 12usize];
    ["Offset of field: vnet_sw_interface_t::hw_if_index"]
        [::std::mem::offset_of!(vnet_sw_interface_t, hw_if_index) - 16usize];
    ["Offset of field: vnet_sw_interface_t::mtu"]
        [::std::mem::offset_of!(vnet_sw_interface_t, mtu) - 20usize];
    ["Offset of field: vnet_sw_interface_t::sub"]
        [::std::mem::offset_of!(vnet_sw_interface_t, sub) - 36usize];
    ["Offset of field: vnet_sw_interface_t::p2p"]
        [::std::mem::offset_of!(vnet_sw_interface_t, p2p) - 48usize];
    ["Offset of field: vnet_sw_interface_t::flood_class"]
        [::std::mem::offset_of!(vnet_sw_interface_t, flood_class) - 64usize];
};
impl Default for vnet_sw_interface_t {
    fn default() -> Self {
        let mut s = ::std::mem::MaybeUninit::<Self>::uninit();
        unsafe {
            ::std::ptr::write_bytes(s.as_mut_ptr(), 0, 1);
            s.assume_init()
        }
    }
}
impl vnet_sw_interface_t {
    #[inline]
    pub fn type_(&self) -> vnet_sw_interface_type_t {
        unsafe { ::std::mem::transmute(self._bitfield_1.get(0usize, 16u8) as u32) }
    }
    #[inline]
    pub fn set_type(&mut self, val: vnet_sw_interface_type_t) {
        unsafe {
            let val: u32 = ::std::mem::transmute(val);
            self._bitfield_1.set(0usize, 16u8, val as u64)
        }
    }
    #[inline]
    pub unsafe fn type__raw(this: *const Self) -> vnet_sw_interface_type_t {
        unsafe {
            ::std::mem::transmute(<__BindgenBitfieldUnit<[u8; 2usize]>>::raw_get(
                ::std::ptr::addr_of!((*this)._bitfield_1),
                0usize,
                16u8,
            ) as u32)
        }
    }
    #[inline]
    pub unsafe fn set_type_raw(this: *mut Self, val: vnet_sw_interface_type_t) {
        unsafe {
            let val: u32 = ::std::mem::transmute(val);
            <__BindgenBitfieldUnit<[u8; 2usize]>>::raw_set(
                ::std::ptr::addr_of_mut!((*this)._bitfield_1),
                0usize,
                16u8,
                val as u64,
            )
        }
    }
    #[inline]
    pub fn new_bitfield_1(type_: vnet_sw_interface_type_t) -> __BindgenBitfieldUnit<[u8; 2usize]> {
        let mut __bindgen_bitfield_unit: __BindgenBitfieldUnit<[u8; 2usize]> = Default::default();
        __bindgen_bitfield_unit.set(0usize, 16u8, {
            let type_: u32 = unsafe { ::std::mem::transmute(type_) };
            type_ as u64
        });
        __bindgen_bitfield_unit
    }
}

// Avoid bindgen-generated code causing this to have an alignment of 8 instead of 1

#[repr(C)]
#[repr(packed)]
#[derive(Copy, Clone)]
pub union ip6_address_t {
    pub as_u8: [u8_; 16usize],
    //    pub as_u16: [u16_; 8usize],
    //    pub as_u32: [u32_; 4usize],
    //    pub as_u64: [u64_; 2usize],
    //    pub as_u128: u64x2,
    //    pub as_uword: [uword; 2usize],
}
#[allow(clippy::unnecessary_operation, clippy::identity_op)]
const _: () = {
    ["Size of ip6_address_t"][::std::mem::size_of::<ip6_address_t>() - 16usize];
    ["Alignment of ip6_address_t"][::std::mem::align_of::<ip6_address_t>() - 1usize];
    ["Offset of field: ip6_address_t::as_u8"]
        [::std::mem::offset_of!(ip6_address_t, as_u8) - 0usize];
    // ["Offset of field: ip6_address_t::as_u16"]
    //     [::std::mem::offset_of!(ip6_address_t, as_u16) - 0usize];
    // ["Offset of field: ip6_address_t::as_u32"]
    //     [::std::mem::offset_of!(ip6_address_t, as_u32) - 0usize];
    // ["Offset of field: ip6_address_t::as_u64"]
    //     [::std::mem::offset_of!(ip6_address_t, as_u64) - 0usize];
    // ["Offset of field: ip6_address_t::as_u128"]
    //     [::std::mem::offset_of!(ip6_address_t, as_u128) - 0usize];
    // ["Offset of field: ip6_address_t::as_uword"]
    //     [::std::mem::offset_of!(ip6_address_t, as_uword) - 0usize];
};
impl Default for ip6_address_t {
    fn default() -> Self {
        let mut s = ::std::mem::MaybeUninit::<Self>::uninit();
        unsafe {
            ::std::ptr::write_bytes(s.as_mut_ptr(), 0, 1);
            s.assume_init()
        }
    }
}

impl<const N: usize> _vlib_node_registration<[*mut ::std::os::raw::c_char; N]> {
    pub const fn new() -> Self {
        let mut s = ::std::mem::MaybeUninit::<Self>::uninit();
        unsafe {
            ::std::ptr::write_bytes(s.as_mut_ptr(), 0, 1);
            s.assume_init()
        }
    }
}
