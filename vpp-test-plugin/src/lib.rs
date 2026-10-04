//! Test VPP plugin
//!

use futures::future::{Either, select};
use lazy_static::lazy_static;
use std::{
    collections::HashSet,
    fmt,
    net::{Ipv4Addr, Ipv6Addr},
    pin::pin,
    ptr::NonNull,
    str::FromStr,
    sync::{LazyLock, Mutex, atomic::AtomicU64},
    time::Duration,
};

use vpp_plugin::{
    ErrorCounters, NextNodes,
    bindings::{
        ip4_header_t, vnet_api_error_t_VNET_API_ERROR_INVALID_VALUE,
        vnet_helper_ip4_header_checksum,
    },
    vlib::{
        self, BufferIndex,
        counter::{CombinedCounter, CombinedCounterIndex, SimpleCounter, SimpleCounterIndex},
        main::sync::BarrierRwLock,
        node::NextNodes,
        node_generic::{
            FeatureNextNode, GenericFeatureNodeX1, GenericFeatureNodeX4, generic_feature_node_x1,
            generic_feature_node_x4,
        },
        process_node::{Receiver, Sender, channel, sleep, timeout},
    },
    vlib_cli_command, vlib_init_function, vlib_node, vlib_plugin_register, vlib_process_node,
    vlibapi,
    vnet::{
        self,
        error::{VNET_ERR_INVALID_ARGUMENT, VnetError},
        types::SwIfIndex,
    },
    vnet_feature_init,
    vppinfra::{error::ErrorStack, unlikely},
};

use crate::test_types_api::{
    TEST_ADDRESS_IP4, TEST_ADDRESS_IP6, TEST_NODE_TYPE_X1, TEST_NODE_TYPE_X4, TestAddressUnion,
    TestIp4Address,
};

mod test_api {
    include!(concat!(env!("OUT_DIR"), "/src/test_api.rs"));
}

mod test_types_api {
    include!(concat!(env!("OUT_DIR"), "/src/test_types_api.rs"));
}

const IP_PROTOCOL_UDP: u8 = 17;

#[repr(C, packed)]
#[derive(Debug, Default, Copy, Clone)]
struct UdpHeader {
    pub src_port: u16,
    pub dst_port: u16,
    pub length: u16,
    pub checksum: u16,
}

#[repr(C, packed)]
#[derive(Debug, Default, Copy, Clone)]
struct IpUdpHeader {
    pub ip: ip4_header_t,
    pub udp: UdpHeader,
}

#[derive(Copy, Clone)]
struct TestTrace {
    header: IpUdpHeader,
}

impl fmt::Display for TestTrace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "header: {:?}", self.header)
    }
}

fn format_test_trace(
    _vm: &mut vlib::MainRef,
    _node: &mut vlib::NodeRef<TestNode>,
    t: &TestTrace,
) -> String {
    t.to_string()
}

fn format_test_trace2(
    _vm: &mut vlib::MainRef,
    _node: &mut vlib::NodeRef<TestX4Node>,
    t: &TestTrace,
) -> String {
    t.to_string()
}

#[derive(NextNodes)]
enum TestNextNode {
    #[next_node = "drop"]
    Drop,
}

#[derive(ErrorCounters)]
enum TestErrorCounter {
    #[error_counter(description = "Drops", severity = ERROR)]
    Drop,
}

#[derive(Copy, Clone)]
struct TestRuntimeData {
    drop_error_ptr: Option<NonNull<u64>>,
}

// SAFETY: this is safe to implement even though drop_error_ptr is thread-local because there are
// no safe methods on TestRuntimeData, so accessing drop_error_ptr is unsafe anyway.
unsafe impl Send for TestRuntimeData {}
// SAFETY: this is safe to implement even though drop_error_ptr is thread-local because there are
// no safe methods on TestRuntimeData, so accessing drop_error_ptr is unsafe anyway.
unsafe impl Sync for TestRuntimeData {}

/// Initialisation data used in node function init
static TEST_RUNTIME_DATA_INIT: TestRuntimeData = TestRuntimeData {
    drop_error_ptr: None,
};

/// Tests using runtime data by incrementing a drop counter, caching the pointer to it
///
/// This probably isn't better for performance and certainly isn't better for maintainability,
/// so don't re-use this without profiling before and after.
fn increment_drop_counter_cached(
    vm: &vlib::MainRef,
    node: &mut vlib::NodeRuntimeRef<TestNode>,
    increment: u64,
) {
    unsafe {
        let node_counter_base_index = (*node.node(vm).as_ptr()).error_heap_index;
        let runtime_data = node.runtime_data_mut();
        let ptr = runtime_data.drop_error_ptr.get_or_insert_with(|| {
            let em = &(*vm.as_ptr()).error_main;
            NonNull::new_unchecked(
                em.counters
                    .add(node_counter_base_index as usize + TestErrorCounter::Drop as usize),
            )
        });
        AtomicU64::from_ptr(ptr.as_ptr()).store(
            *ptr.as_ptr() + increment,
            std::sync::atomic::Ordering::Relaxed,
        );
    }
}

static TEST_NODE: TestNode = TestNode::new();

lazy_static! {
    static ref SIMPLE_COUNTER: SimpleCounter =
        SimpleCounter::new("test-simple", "/net/test/simple");
    static ref COMBINED_COUNTER: CombinedCounter =
        CombinedCounter::new("test-combined", "/net/test/combined");
}

#[vlib_node(
    name = "test",
    instance = TEST_NODE,
    runtime_data_default = TEST_RUNTIME_DATA_INIT,
    format_trace = format_test_trace,
)]
struct TestNode {
    udp_src_port_deny_policy: BarrierRwLock<Option<u16>>,
}

impl TestNode {
    const fn new() -> Self {
        Self {
            udp_src_port_deny_policy: BarrierRwLock::new(None),
        }
    }
}

impl vlib::node::Node for TestNode {
    type Vector = BufferIndex;
    type Scalar = ();
    type Aux = ();

    type NextNodes = TestNextNode;
    type RuntimeData = TestRuntimeData;
    type TraceData = TestTrace;
    type Errors = TestErrorCounter;
    type FeatureData = ();

    #[inline(always)]
    unsafe fn function(
        &self,
        vm: &mut vlib::MainRef,
        node: &mut vlib::NodeRuntimeRef<Self>,
        frame: &mut vlib::FrameRef<Self>,
    ) -> u16 {
        unsafe {
            struct Impl;
            impl GenericFeatureNodeX1<TestNode> for Impl {
                #[inline(always)]
                unsafe fn map_buffer_to_next(
                    &self,
                    vm: &vlib::MainRef,
                    node: &mut vlib::NodeRuntimeRef<TestNode>,
                    b0: &mut vlib::BufferRef<()>,
                ) -> FeatureNextNode<TestNextNode> {
                    unsafe {
                        if usize::from(b0.current_length()) < std::mem::size_of::<IpUdpHeader>() {
                            b0.set_error(node, TestErrorCounter::Drop);
                            return TestNextNode::Drop.into();
                        }

                        let ip_udp: *const IpUdpHeader = b0.current_ptr_mut() as *const IpUdpHeader;

                        let next = match u16::from_be((*ip_udp).udp.dst_port) {
                            // 1 falls through into default case to test the simple case
                            2 => FeatureNextNode::NextFeature,
                            3 => {
                                node.increment_error_counter(vm, TestErrorCounter::Drop, 1);
                                TestNextNode::Drop.into()
                            }
                            4 => {
                                increment_drop_counter_cached(vm, node, 1);
                                TestNextNode::Drop.into()
                            }
                            5 => {
                                SimpleCounterIndex::from_parts(&SIMPLE_COUNTER, 0).increment(vm, 1);
                                FeatureNextNode::NextFeature
                            }
                            6 => {
                                CombinedCounterIndex::from_parts(&COMBINED_COUNTER, 0).increment(
                                    vm,
                                    1,
                                    b0.length_in_chain(vm),
                                );
                                FeatureNextNode::NextFeature
                            }
                            7 => {
                                if let Some(src_port) = *TEST_NODE.udp_src_port_deny_policy.read(vm)
                                {
                                    if u16::from_be((*ip_udp).udp.src_port) == src_port {
                                        b0.set_error(node, TestErrorCounter::Drop);
                                        TestNextNode::Drop.into()
                                    } else {
                                        FeatureNextNode::NextFeature
                                    }
                                } else {
                                    FeatureNextNode::NextFeature
                                }
                            }
                            _ => {
                                b0.set_error(node, TestErrorCounter::Drop);
                                TestNextNode::Drop.into()
                            }
                        };

                        if unlikely(b0.flags().contains(vlib::BufferFlags::IS_TRACED)) {
                            let t = b0.add_trace(vm, node);
                            t.write(TestTrace { header: *ip_udp });
                        }

                        next
                    }
                }
            }
            generic_feature_node_x1(vm, node, frame, Impl)
        }
    }
}

vnet_feature_init! {
    identifier: TEST_FEAT,
    arc_name: "ip4-unicast",
    node: TestNode,
}

static TESTX4_NODE: TestX4Node = TestX4Node::new();

#[vlib_node(
    name = "testx4",
    instance = TESTX4_NODE,
    format_trace = format_test_trace2,
)]
struct TestX4Node;

impl TestX4Node {
    const fn new() -> Self {
        Self
    }
}

impl vlib::node::Node for TestX4Node {
    type Vector = BufferIndex;
    type Scalar = ();
    type Aux = ();

    type NextNodes = TestNextNode;
    type RuntimeData = ();
    type TraceData = TestTrace;
    type Errors = TestErrorCounter;
    type FeatureData = ();

    #[inline(always)]
    unsafe fn function(
        &self,
        vm: &mut vlib::MainRef,
        node: &mut vlib::NodeRuntimeRef<Self>,
        frame: &mut vlib::FrameRef<Self>,
    ) -> u16 {
        unsafe {
            struct Impl;

            impl GenericFeatureNodeX4<TestX4Node> for Impl {
                fn prefetch_buffer_x4(
                    &self,
                    _vm: &vlib::MainRef,
                    _node: &mut vlib::NodeRuntimeRef<TestX4Node>,
                    b: &mut [&mut vlib::BufferRef<<TestX4Node as vlib::node::Node>::FeatureData>;
                             4],
                ) {
                    b.iter().for_each(|b0| {
                        b0.prefetch_header_load();
                        b0.prefetch_data_load();
                    });
                }

                #[inline(always)]
                unsafe fn map_buffer_to_next_x4(
                    &self,
                    vm: &vlib::MainRef,
                    node: &mut vlib::NodeRuntimeRef<TestX4Node>,
                    b: &mut [&mut vlib::BufferRef<()>; 4],
                ) -> [FeatureNextNode<TestNextNode>; 4] {
                    unsafe {
                        [
                            self.map_buffer_to_next(vm, node, b[0]),
                            self.map_buffer_to_next(vm, node, b[1]),
                            self.map_buffer_to_next(vm, node, b[2]),
                            self.map_buffer_to_next(vm, node, b[3]),
                        ]
                    }
                }

                unsafe fn trace_buffer(
                    &self,
                    vm: &vlib::MainRef,
                    node: &mut vlib::NodeRuntimeRef<TestX4Node>,
                    b0: &mut vlib::BufferRef<<TestX4Node as vlib::node::Node>::FeatureData>,
                ) {
                    unsafe {
                        let ip_udp = b0.current_ptr_mut() as *const IpUdpHeader;
                        if usize::from(b0.current_length()) >= std::mem::size_of::<IpUdpHeader>() {
                            let t = b0.add_trace(vm, node);
                            t.write(TestTrace { header: *ip_udp });
                        }
                    }
                }
            }

            impl GenericFeatureNodeX1<TestX4Node> for Impl {
                #[inline(always)]
                unsafe fn map_buffer_to_next(
                    &self,
                    _vm: &vlib::MainRef,
                    node: &mut vlib::NodeRuntimeRef<TestX4Node>,
                    b0: &mut vlib::BufferRef<()>,
                ) -> FeatureNextNode<TestNextNode> {
                    unsafe {
                        if usize::from(b0.current_length()) < std::mem::size_of::<IpUdpHeader>() {
                            b0.set_error(node, TestErrorCounter::Drop);
                            return TestNextNode::Drop.into();
                        }

                        let ip_udp = b0.current_ptr_mut() as *const IpUdpHeader;

                        match u16::from_be((*ip_udp).udp.dst_port) {
                            1 => {
                                b0.set_error(node, TestErrorCounter::Drop);
                                TestNextNode::Drop.into()
                            }
                            _ => FeatureNextNode::NextFeature,
                        }
                    }
                }
            }

            generic_feature_node_x4(vm, node, frame, Impl)
        }
    }
}

vnet_feature_init! {
    identifier: TESTX4_FEAT,
    arc_name: "ip4-unicast",
    node: TestX4Node,
}

/// Interfaces `testchain` is enabled on, as a bitmap of sw_if_index (tests use fewer than 64).
static TEST_CHAIN_ENABLED: AtomicU64 = AtomicU64::new(0);

#[derive(ErrorCounters)]
enum TestChainCounter {
    #[error_counter(description = "From interfaces the feature is enabled on", severity = INFO)]
    Enabled,
    #[error_counter(description = "From interfaces the feature is not enabled on", severity = ERROR)]
    Foreign,
}

static TEST_CHAIN_NODE: TestChainNode = TestChainNode::new();

/// A feature that runs after `testx4` and only counts, by whether the packet's rx interface has
/// it enabled. Packets reaching it from an interface without it were misrouted by an earlier
/// feature; a packet counted twice was routed back to it.
#[vlib_node(name = "testchain", instance = TEST_CHAIN_NODE)]
struct TestChainNode;

impl TestChainNode {
    const fn new() -> Self {
        Self
    }
}

impl vlib::node::Node for TestChainNode {
    type Vector = BufferIndex;
    type Scalar = ();
    type Aux = ();

    type NextNodes = TestNextNode;
    type RuntimeData = ();
    type TraceData = ();
    type Errors = TestChainCounter;
    type FeatureData = ();

    #[inline(always)]
    unsafe fn function(
        &self,
        vm: &mut vlib::MainRef,
        node: &mut vlib::NodeRuntimeRef<Self>,
        frame: &mut vlib::FrameRef<Self>,
    ) -> u16 {
        unsafe {
            struct Impl;

            impl GenericFeatureNodeX1<TestChainNode> for Impl {
                #[inline(always)]
                unsafe fn map_buffer_to_next(
                    &self,
                    vm: &vlib::MainRef,
                    node: &mut vlib::NodeRuntimeRef<TestChainNode>,
                    b0: &mut vlib::BufferRef<()>,
                ) -> FeatureNextNode<TestNextNode> {
                    let sw_if_index = u32::from(b0.vnet_buffer().rx_sw_if_index());
                    let enabled = sw_if_index < 64
                        && TEST_CHAIN_ENABLED.load(std::sync::atomic::Ordering::Relaxed)
                            & (1 << sw_if_index)
                            != 0;
                    let counter = if enabled {
                        TestChainCounter::Enabled
                    } else {
                        TestChainCounter::Foreign
                    };
                    node.increment_error_counter(vm, counter, 1);
                    FeatureNextNode::NextFeature
                }
            }

            generic_feature_node_x1(vm, node, frame, Impl)
        }
    }
}

vnet_feature_init! {
    identifier: TESTCHAIN_FEAT,
    arc_name: "ip4-unicast",
    node: TestChainNode,
    runs_after: ["testx4"],
}

#[vlib_cli_command(
    path = "rust-test chain",
    short_help = "rust-test chain <interface-name> [disable]"
)]
fn chain_enable_disable_command(
    vm: &mut vlib::BarrierHeldMainRef,
    input: &str,
) -> Result<(), ErrorStack> {
    let args: Vec<_> = input.split_whitespace().collect();
    let name = args
        .first()
        .ok_or_else(|| ErrorStack::msg("Missing interface name"))?;
    let sw_if_index = SwIfIndex::from_str(name)
        .map_err(|_| ErrorStack::msg(format!("Invalid interface name {name}")))?;
    let bit = 1u64
        .checked_shl(u32::from(sw_if_index))
        .filter(|_| u32::from(sw_if_index) < 64)
        .ok_or_else(|| ErrorStack::msg("sw_if_index too large for the test bitmap"))?;
    let enable = match args.get(1) {
        None => true,
        Some(&"disable") => false,
        Some(other) => return Err(ErrorStack::msg(format!("Unrecognised option {other}"))),
    };

    if enable {
        TEST_CHAIN_ENABLED.fetch_or(bit, std::sync::atomic::Ordering::Relaxed);
        TESTCHAIN_FEAT
            .enable(vm, sw_if_index, ())
            .map_err(|e| e.context("Failed to enable chain feature"))?;
    } else {
        TESTCHAIN_FEAT
            .disable(vm, sw_if_index)
            .map_err(|e| e.context("Failed to disable chain feature"))?;
        TEST_CHAIN_ENABLED.fetch_and(!bit, std::sync::atomic::Ordering::Relaxed);
    }

    Ok(())
}

#[vlib_cli_command(
    path = "rust-test node",
    short_help = "rust-test node <interface-name> [disable]"
)]
fn enable_disable_command(
    vm: &mut vlib::BarrierHeldMainRef,
    input: &str,
) -> Result<(), ErrorStack> {
    let args: Vec<_> = input.split(' ').collect();
    if args.is_empty() {
        return Err(ErrorStack::msg("Missing interface name"));
    }

    let mut enable = true;
    let sw_if_index = SwIfIndex::from_str(args[0])
        .map_err(|_| ErrorStack::msg(format!("Invalid interface name {}", args[0])))?;

    if args.len() >= 2 {
        if args[1] == "disable" {
            enable = false;
        } else {
            return Err(ErrorStack::msg(format!("Unrecognised option {}", args[1])));
        }
    }

    if enable {
        TEST_FEAT
            .enable(vm, sw_if_index, ())
            .map_err(|e| e.context("Failed to enable test feature"))?;
    } else {
        TEST_FEAT
            .disable(vm, sw_if_index)
            .map_err(|e| e.context("Failed to disable test feature"))?;
    }

    Ok(())
}

#[vlib_cli_command(
    path = "rust-test negative",
    short_help = "rust-test negative <vnet-error>"
)]
fn negative_test_command(
    _vm: &mut vlib::BarrierHeldMainRef,
    input: &str,
) -> Result<(), ErrorStack> {
    if input == "vnet-error" {
        return Err(VnetError::from(vnet_api_error_t_VNET_API_ERROR_INVALID_VALUE).context("Test"));
    } else {
        return Err(ErrorStack::msg(format!("Unrecognised input {}", input)));
    }
}

#[vlib_cli_command(path = "rust-test message", short_help = "rust-test message")]
fn message_test_command(
    _vm: &mut vlib::BarrierHeldMainRef,
    _input: &str,
) -> Result<(), ErrorStack> {
    // Test some basic operations not tested from the auto-generated API code
    let mut message = vlibapi::Message::from(0u8);
    if *message != 0 {
        return Err(ErrorStack::msg(format!(
            "Expected *message to be 0, but is {}",
            message
        )));
    }
    *message = 1;
    if *message != 1 {
        return Err(ErrorStack::msg(format!(
            "Expected *message to be 1, but is {}",
            message
        )));
    }
    // Test format implementations
    println!(
        "message_test_command({:p} -> {} ({:?}))",
        message, message, message
    );

    // Test Ord & Eq implementations
    let message_greater = vlibapi::Message::from(2u8);
    if message_greater <= message {
        return Err(ErrorStack::msg(format!(
            "Expected message {} to be > {}, but it wasn't",
            message, message_greater
        )));
    }
    let message2 = vlibapi::Message::from(1u8);
    if message2 != message {
        return Err(ErrorStack::msg(format!(
            "Expected message {} to be == {}, but it wasn't",
            message, message2
        )));
    }

    // Test Hash implementation
    let mut message_set = HashSet::new();
    message_set.insert(message);
    let message = vlibapi::Message::from(1u8);
    if !message_set.contains(&message) {
        return Err(ErrorStack::msg(format!(
            "Expected message {} to be in message_set {:?}, but isn't",
            message, message_set
        )));
    }

    // Test functions for partial initialisation
    let mut message = vlibapi::Message::<[u8; 256]>::new_uninit();
    unsafe {
        for i in 0..256 {
            *(message.as_mut_ptr().add(i) as *mut u8) = 0;
        }
        let message = message.assume_init();
        if *message != [0u8; 256] {
            return Err(ErrorStack::msg(format!(
                "Expected *message to be [0u8; 256], but is {:?}",
                message
            )));
        }
    }
    let message = vlibapi::Message::<u8>::new_uninit();
    let message = message.write(0);
    if *message != 0 {
        return Err(ErrorStack::msg(format!(
            "Expected *message to be 0, but is {}",
            message
        )));
    }

    Ok(())
}

#[vlib_cli_command(
    path = "rust-test counter",
    short_help = "rust-test counter <simple|combined>"
)]
fn counter_test_command(vm: &mut vlib::BarrierHeldMainRef, input: &str) -> Result<(), ErrorStack> {
    if input == "simple" {
        let counter = SimpleCounter::new("ut-simple", "/net/ut/simple");
        let counter_index = counter.allocate_index(vm, 0);
        let (counter_ref, index) = counter_index.into_parts();
        let counter_index = unsafe { SimpleCounterIndex::from_parts(counter_ref, index) };
        counter_index.increment(vm, 1);
        let counter_val = counter_index.get(vm);
        if counter_val != 1 {
            return Err(ErrorStack::msg(format!(
                "Expected counter value to be 1 instead of {}",
                counter_val
            )));
        }
        unsafe {
            counter_index.zero();
        }
        let counter_val = counter_index.get(vm);
        if counter_val != 0 {
            return Err(ErrorStack::msg(format!(
                "Expected counter value to be 0 instead of {}",
                counter_val
            )));
        }
    } else if input == "combined" {
        let counter = CombinedCounter::new("ut-combined", "/net/ut/combined");
        let counter_index = counter.allocate_index(vm, 0);
        let (counter_ref, index) = counter_index.into_parts();
        let counter_index = unsafe { CombinedCounterIndex::from_parts(counter_ref, index) };
        counter_index.increment(vm, 1, 64); // Increment by 1 packet and 64 bytes
        let counter_val = counter_index.get(vm);
        if counter_val.packets != 1 || counter_val.bytes != 64 {
            return Err(ErrorStack::msg(format!(
                "Expected counter value to be (1, 64) instead of ({}, {})",
                counter_val.packets, counter_val.bytes
            )));
        }
        unsafe {
            counter_index.zero();
        }
        let counter_val = counter_index.get(vm);
        if counter_val.packets != 0 || counter_val.bytes != 0 {
            return Err(ErrorStack::msg(format!(
                "Expected counter value to be (0, 0) instead of ({}, {})",
                counter_val.packets, counter_val.bytes
            )));
        }
    } else {
        return Err(ErrorStack::msg(format!("Unrecognised input {}", input)));
    }

    Ok(())
}

impl std::fmt::Debug for test_types_api::TestIp4Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ipv4Addr::from_octets(self.0).fmt(f)
    }
}

#[allow(clippy::derivable_impls)]
impl Default for test_types_api::TestIp4Address {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl PartialEq for test_types_api::TestIp4Address {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::fmt::Debug for test_types_api::TestIp6Address {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        Ipv6Addr::from_octets(self.0).fmt(f)
    }
}

#[allow(clippy::derivable_impls)]
impl Default for test_types_api::TestIp6Address {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl PartialEq for test_types_api::TestIp6Address {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl std::fmt::Debug for test_types_api::TestAddress {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe {
            if self.af == TEST_ADDRESS_IP4 {
                self.un.ip4.fmt(f)
            } else if self.af == TEST_ADDRESS_IP6 {
                self.un.ip6.fmt(f)
            } else {
                write!(f, "Unknown af {:?}", self.af)
            }
        }
    }
}

#[allow(clippy::derivable_impls)]
impl Default for test_types_api::TestAddress {
    fn default() -> Self {
        Self {
            af: Default::default(),
            un: TestAddressUnion {
                ip4: TestIp4Address::default(),
            },
        }
    }
}

impl PartialEq for test_types_api::TestAddress {
    fn eq(&self, other: &Self) -> bool {
        if self.af != other.af {
            return false;
        }
        unsafe {
            if self.af == TEST_ADDRESS_IP4 {
                self.un.ip4 == other.un.ip4
            } else if self.af == TEST_ADDRESS_IP6 {
                self.un.ip6 == other.un.ip6
            } else {
                false
            }
        }
    }
}

impl ::vpp_plugin::vlibapi::EndianSwap for test_types_api::TestAddressUnion {
    unsafe fn endian_swap(&mut self, to_net: bool) {
        let _ = to_net;
        // no-op
    }
}

struct ApiHandler;

impl test_api::Handlers for ApiHandler {
    fn test_enable_disable(
        vm: &vpp_plugin::vlib::BarrierHeldMainRef,
        mp: &test_api::TestEnableDisable,
    ) -> Result<vlibapi::Message<test_api::TestEnableDisableReply>, i32> {
        let sw_if_index = SwIfIndex::new(mp.sw_if_index);

        match mp.node_type {
            TEST_NODE_TYPE_X1 => {
                if mp.enable {
                    TEST_FEAT.enable(vm, sw_if_index, ())?;
                } else {
                    TEST_FEAT.disable(vm, sw_if_index)?;
                }
            }
            TEST_NODE_TYPE_X4 => {
                if mp.enable {
                    TESTX4_FEAT.enable(vm, sw_if_index, ())?;
                } else {
                    TESTX4_FEAT.disable(vm, sw_if_index)?;
                }
            }
            _ => return Err(VNET_ERR_INVALID_ARGUMENT.into()),
        }

        Ok(Default::default())
    }

    fn test_type_in_message(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestTypeInMessage,
    ) -> Result<vlibapi::Message<test_api::TestTypeInMessageReply>, i32> {
        if mp.test_type.field1 != 42 {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        Ok(Default::default())
    }

    fn test_array(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestArray,
    ) -> Result<vlibapi::Message<test_api::TestArrayReply>, i32> {
        let array = mp.array1;
        if array != [42, 0xdeadbeef, 0, 0xffffffff] {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        let array = mp.array2.array;
        if array != [42, 0xffff] {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        let array = mp.array3;
        if array != [TEST_NODE_TYPE_X1, TEST_NODE_TYPE_X4] {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        if mp.array4[0] != 0.0 || mp.array4[1] != 42.0 {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        Ok(Default::default())
    }

    fn test_response(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestResponse,
    ) -> Result<vlibapi::Message<test_api::TestResponseReply>, i32> {
        Ok(test_api::TestResponseReply {
            value: mp.value,
            ..Default::default()
        }
        .into())
    }

    fn test_response_no_retval(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestResponseNoRetval,
    ) -> vlibapi::Message<test_api::TestResponseNoRetvalReply> {
        test_api::TestResponseNoRetvalReply {
            value: mp.value,
            ..Default::default()
        }
        .into()
    }

    fn test_dump(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestDump,
        mut stream: vlibapi::Stream<test_api::TestDetails>,
    ) {
        unsafe {
            stream.send_message(
                test_api::TestDetails {
                    context: mp.context,
                    value: 1,
                    ..Default::default()
                }
                .into(),
            );
            stream.send_message(
                test_api::TestDetails {
                    context: mp.context,
                    value: 2,
                    ..Default::default()
                }
                .into(),
            );
        }
    }

    fn test_stream_get(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestStreamGet,
        mut stream: ::vpp_plugin::vlibapi::Stream<test_api::TestStreamDetails>,
    ) -> Result<vlibapi::Message<test_api::TestStreamGetReply>, i32> {
        unsafe {
            stream.send_message(
                test_api::TestStreamDetails {
                    context: mp.context,
                    value: 1,
                    ..Default::default()
                }
                .into(),
            );
            stream.send_message(
                test_api::TestStreamDetails {
                    context: mp.context,
                    value: 2,
                    ..Default::default()
                }
                .into(),
            );
        }
        Ok(Default::default())
    }

    fn test_typedef(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestTypedef,
    ) -> Result<vlibapi::Message<test_api::TestTypedefReply>, i32> {
        if mp.addr.0.0 != [1, 2, 3, 4] {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        Ok(Default::default())
    }

    fn test_union(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestUnion,
    ) -> Result<vlibapi::Message<test_api::TestUnionReply>, i32> {
        unsafe {
            if mp.addr.af == TEST_ADDRESS_IP4 {
                if mp.addr.un.ip4.0 != [1, 2, 3, 4] {
                    return Err(VNET_ERR_INVALID_ARGUMENT.into());
                }
            } else if mp.addr.af == TEST_ADDRESS_IP6 {
                if mp.addr.un.ip6.0 != [1, 2, 3, 4, 5, 6, 7, 8, 9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf, 0]
                {
                    return Err(VNET_ERR_INVALID_ARGUMENT.into());
                }
            } else {
                return Err(VNET_ERR_INVALID_ARGUMENT.into());
            }
        }
        Ok(Default::default())
    }

    unsafe fn test_variable_array_u32(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestVariableArrayU32,
    ) -> Result<vlibapi::Message<test_api::TestVariableArrayU32Reply>, i32> {
        unsafe {
            println!("test_variable_array_u32({:?}, {:?})", mp, mp.values());
            if mp.values() != [42, 0xdeadbeef, 0, 0xffffffff] {
                return Err(VNET_ERR_INVALID_ARGUMENT.into());
            }
            let mut reply = test_api::TestVariableArrayU32Reply::new_message(mp.nitems.into());
            for (src, dest) in mp.values().iter().zip(reply.values_mut()) {
                *dest = *src;
            }
            println!(
                "test_variable_array_u32() <- {:?}, {:?}",
                reply,
                reply.values()
            );
            Ok(reply)
        }
    }

    unsafe fn test_variable_array_u8(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestVariableArrayU8,
    ) -> Result<vlibapi::Message<test_api::TestVariableArrayU8Reply>, i32> {
        unsafe {
            println!("test_variable_array_u8({:?}, {:?})", mp, mp.values());
            if mp.values() != [42, 0, 0xff] {
                return Err(VNET_ERR_INVALID_ARGUMENT.into());
            }
            Ok(Default::default())
        }
    }

    unsafe fn test_variable_array_f64(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestVariableArrayF64,
    ) -> Result<vlibapi::Message<test_api::TestVariableArrayF64Reply>, i32> {
        unsafe {
            println!("test_variable_array_f64({:?}, {:?})", mp, mp.values());
            if mp.values() != [0.0, 42.0] {
                return Err(VNET_ERR_INVALID_ARGUMENT.into());
            }
            Ok(Default::default())
        }
    }

    unsafe fn test_variable_array_custom(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestVariableArrayCustom,
    ) -> Result<vlibapi::Message<test_api::TestVariableArrayCustomReply>, i32> {
        unsafe {
            println!("test_variable_array_custom({:?}, {:?})", mp, mp.values());
            if mp.values() != [TEST_NODE_TYPE_X1, TEST_NODE_TYPE_X4] {
                return Err(VNET_ERR_INVALID_ARGUMENT.into());
            }
            Ok(Default::default())
        }
    }

    unsafe fn test_variable_array_in_type(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestVariableArrayInType,
    ) -> Result<vlibapi::Message<test_api::TestVariableArrayInTypeReply>, i32> {
        unsafe {
            println!("test_variable_array_u32({:?}, {:?})", mp, mp.field.values());
            if mp.field.values() != [42, 0xdead, 0, 0xffff] {
                return Err(VNET_ERR_INVALID_ARGUMENT.into());
            }
            Ok(Default::default())
        }
    }

    unsafe fn test_string(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestString,
    ) -> Result<vlibapi::Message<test_api::TestStringReply>, i32> {
        println!("test_string({:?})", mp);
        if mp.fixed.to_string_lossy() != format!("{:<63}", "Hello World!") {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        if mp.variable.to_string_lossy() != "Hello World!" {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        // Test some other ApiString methods
        if mp
            .variable
            .to_str()
            .map_err(|_| VNET_ERR_INVALID_ARGUMENT)?
            != "Hello World!"
        {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        if mp.variable.is_empty() {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }

        let reply_str = "Goodbye World!";
        let mut reply = test_api::TestStringReply::new_message(reply_str.len() as u32);
        reply.fixed.copy_from_str(&format!("{:<64}", reply_str));
        reply.variable.copy_from_str(reply_str);
        Ok(reply)
    }

    fn test_enumflag(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestEnumflag,
    ) -> Result<vlibapi::Message<test_api::TestEnumflagReply>, i32> {
        println!("test_enumflag({:?})", mp);
        if mp.flags != test_types_api::TestDir::RX | test_types_api::TestDir::TX {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        if mp.flags2 != test_types_api::TestDir2::RX | test_types_api::TestDir2::TX {
            return Err(VNET_ERR_INVALID_ARGUMENT.into());
        }
        Ok(Default::default())
    }

    fn test_barrier_rw_lock(
        vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestBarrierRwLock,
    ) -> Result<vlibapi::Message<test_api::TestBarrierRwLockReply>, i32> {
        if mp.enable {
            *TEST_NODE.udp_src_port_deny_policy.write(vm) = Some(7);
        } else {
            *TEST_NODE.udp_src_port_deny_policy.write(vm) = None;
        }
        Ok(Default::default())
    }

    fn test_process_node(
        _vm: &vlib::BarrierHeldMainRef,
        mp: &test_api::TestProcessNode,
    ) -> Result<vlibapi::Message<test_api::TestProcessNodeReply>, i32> {
        if mp.enable {
            TEST_PROCESS_NODE
                .channel_sender()
                .send(ProcessMessage::Enable(Ipv4Addr::from_octets(mp.dest.0)))
                .map_err(|_| VNET_ERR_INVALID_ARGUMENT)?;
        } else {
            TEST_PROCESS_NODE
                .channel_sender()
                .send(ProcessMessage::Disable)
                .map_err(|_| VNET_ERR_INVALID_ARGUMENT)?;
        }
        Ok(Default::default())
    }
}

static TEST_PROCESS_NODE: TestProcessNode = TestProcessNode::new();

#[derive(Debug)]
enum ProcessMessage {
    Enable(Ipv4Addr),
    Disable,
}

type GetOnceProcessMessageReceiver = Mutex<Option<Receiver<ProcessMessage>>>;

#[derive(NextNodes)]
enum TestProcessNextNode {
    #[next_node = "ip4-lookup"]
    Ip4Lookup,
}

#[derive(ErrorCounters)]
enum TestProcessErrorCounter {
    #[error_counter(description = "Buffer allocation", severity = ERROR)]
    BufferAllocation,
}

#[vlib_process_node(
    name = "test-process",
    instance = TEST_PROCESS_NODE,
    // Determined by trial-and-error - the default log2_stack_bytes of 15 isn't sufficient in the
    // arm64 tests.
    log2_stack_bytes = 16,
)]
struct TestProcessNode {
    channel: LazyLock<(Sender<ProcessMessage>, GetOnceProcessMessageReceiver)>,
}

impl TestProcessNode {
    const fn new() -> Self {
        Self {
            channel: LazyLock::new(|| {
                let (sender, receiver) = channel();
                (sender, Mutex::new(Some(receiver)))
            }),
        }
    }

    fn channel_sender(&self) -> Sender<ProcessMessage> {
        self.channel.0.clone()
    }

    fn channel_receiver(&self) -> Receiver<ProcessMessage> {
        self.channel.1.lock().unwrap().take().unwrap()
    }

    fn send_packet(
        &self,
        vm: &mut vlib::MainRef,
        node: &mut vlib::NodeRuntimeRef<Self>,
        destination: Ipv4Addr,
    ) {
        // Test dropping a buffer
        drop(match vm.alloc_buffer() {
            Ok(buffer) => buffer,
            Err(_) => {
                node.increment_process_error_counter(
                    vm,
                    TestProcessErrorCounter::BufferAllocation,
                    1,
                );
                return;
            }
        });

        let mut buffer = match vm.alloc_buffer() {
            Ok(buffer) => buffer,
            Err(_) => {
                node.increment_process_error_counter(
                    vm,
                    TestProcessErrorCounter::BufferAllocation,
                    1,
                );
                return;
            }
        };
        let next = TestProcessNextNode::Ip4Lookup;
        let b = buffer.as_buffer_ref();
        b.vnet_buffer_mut().set_rx_sw_if_index(SwIfIndex::LOCAL0);
        // Test accessor
        assert_eq!(b.vnet_buffer().rx_sw_if_index(), SwIfIndex::LOCAL0);
        b.vnet_buffer_mut().set_tx_sw_if_index(None);
        // Test accessor
        assert_eq!(b.vnet_buffer().tx_sw_if_index(), None);
        unsafe {
            b.set_flags(
                (b.flags().vnet_flags() | vnet::buffer::BufferFlags::LOCALLY_ORIGINATED)
                    .as_vlib_flags(),
            );
            // Verify that there is enough space, but only in tests
            let buffer_default_data_size = vm.buffer_default_data_size();
            assert!(buffer_default_data_size >= std::mem::size_of::<IpUdpHeader>() as u32);
            let ip_udp =
                b.put_uninit(std::mem::size_of::<IpUdpHeader>() as u16) as *mut IpUdpHeader;
            (*ip_udp).ip = std::mem::zeroed();
            (*ip_udp).ip.__bindgen_anon_1.ip_version_and_header_length = 0x45;
            (*ip_udp).ip.__bindgen_anon_1.ttl = 128;
            (*ip_udp).ip.__bindgen_anon_1.protocol = IP_PROTOCOL_UDP;
            (*ip_udp).ip.__bindgen_anon_1.length =
                (std::mem::size_of::<UdpHeader>() as u16).to_be();
            (*ip_udp)
                .ip
                .__bindgen_anon_1
                .__bindgen_anon_1
                .__bindgen_anon_1
                .dst_address
                .as_u32 = destination.to_bits().to_be();
            (*ip_udp).ip.__bindgen_anon_1.checksum =
                vnet_helper_ip4_header_checksum(std::ptr::addr_of_mut!((*ip_udp).ip));
            (*ip_udp).udp = UdpHeader {
                src_port: 8u16.to_be(),
                dst_port: 8u16.to_be(),
                length: 0,
                checksum: 0,
            };
        }
        let (buffer_index, _) = buffer.into_parts();
        unsafe {
            vm.buffer_enqueue_to_next(node, &[buffer_index], &[next.into_u16()]);
        }
    }
}

impl vlib::ProcessNode for TestProcessNode {
    type NextNodes = TestProcessNextNode;

    type RuntimeData = ();

    type Errors = TestProcessErrorCounter;

    async fn function(&self, vm: &mut vlib::MainRef, node: &mut vlib::NodeRuntimeRef<Self>) {
        // Verify operation of is_elapsed method
        let sleep1 = sleep(Duration::from_secs(0));
        assert!(sleep1.is_elapsed());

        // Verify some timeout methods
        let res = timeout(Duration::from_secs(0), sleep(Duration::from_secs(u64::MAX))).await;
        let e = res.unwrap_err();
        assert_eq!(e.to_string(), "deadline has elapsed");
        let mut timeout1 = timeout(Duration::from_secs(0), sleep(Duration::from_secs(u64::MAX)));
        assert!(!timeout1.get_mut().is_elapsed());
        assert!(!timeout1.get_ref().is_elapsed());
        assert!(!timeout1.into_inner().is_elapsed());

        let recv = self.channel_receiver();
        let mut destination = None;
        // Pre-create the packet send timer so that it's outside the loop.
        //
        // If it was inside the loop then any time a message was received on the channel, the
        // timer would reset, which isn't the behaviour we want.
        let packet_send_timer = sleep(Duration::from_secs(u64::MAX));
        // Pin packet_send_timer to tell select below that we promise not to move it to a
        // different location in memory in between calls to the select function
        let mut packet_send_timer = pin!(packet_send_timer);
        loop {
            let message = if let Some(destination) = destination {
                println!("waiting for an event message for up to 100 ms...");
                match select(recv.recv(), &mut packet_send_timer).await {
                    Either::Left((message, _)) => message,
                    Either::Right(_) => {
                        println!("... sending packet");
                        self.send_packet(vm, node, destination);
                        // Reset timer so it will fire continuously
                        packet_send_timer.set(sleep(Duration::from_millis(100)));
                        continue;
                    }
                }
            } else {
                recv.recv().await
            };
            match message {
                Some(ProcessMessage::Enable(new_dest)) => {
                    destination = Some(new_dest);
                    // Start timer
                    packet_send_timer.set(sleep(Duration::from_millis(100)));
                }
                Some(ProcessMessage::Disable) => destination = None,
                None => unreachable!(
                    "The sender should never be dropped as it's part of the TEST_PROCESS_NODE singleton"
                ),
            }
        }
    }
}

#[vlib_init_function]
fn test_init(vm: &mut vlib::BarrierHeldMainRef) -> Result<(), ErrorStack> {
    test_api::test_register_messages::<ApiHandler>();
    SIMPLE_COUNTER.allocate_index(vm, 0);
    COMBINED_COUNTER.allocate_index(vm, 0);

    Ok(())
}

vlib_plugin_register! {
    version: "1.0",
    description: "Test",
    version_required: vpp_plugin::VPP_BUILD_VER,
}
