#!/usr/bin/env python3
""" Integration tests """

from socket import AF_INET, inet_pton
from time import sleep
import unittest

from scapy.layers.inet import IP
from scapy.layers.l2 import Ether
from scapy.layers.inet import UDP

from framework import VppTestCase
from asfframework import (
    tag_run_solo,
    VppTestRunner,
)
from util import ppp
from vpp_papi_provider import CliFailedCommandError
from vpp_papi import VppEnum


@tag_run_solo
class IntegrationTestCase(VppTestCase):
    """Integration tests"""

    pg0 = None
    pg1 = None

    @classmethod
    def setUpClass(cls):
        super(IntegrationTestCase, cls).setUpClass()
        cls.__doc__ = (
            """Integration tests"""
        )
        try:
            cls.create_pg_interfaces([0, 1])
            cls.pg0.config_ip4()
            cls.pg0.config_ip6()
            cls.pg0.configure_ipv4_neighbors()
            cls.pg0.admin_up()
            cls.pg0.resolve_arp()
            cls.pg0.resolve_ndp()
            cls.pg1.config_ip4()
            cls.pg1.config_ip6()
            cls.pg1.configure_ipv4_neighbors()
            cls.pg1.admin_up()
            cls.pg1.resolve_arp()
            cls.pg1.resolve_ndp()

        except Exception:
            super(IntegrationTestCase, cls).tearDownClass()
            raise

    @classmethod
    def tearDownClass(cls):
        super(IntegrationTestCase, cls).tearDownClass()

    def setUp(self):
        super(IntegrationTestCase, self).setUp()
        self.pg0.enable_capture()
        self.pg1.enable_capture()

    def tearDown(self):
        self.vapi.collect_events()  # clear the event queue
        super(IntegrationTestCase, self).tearDown()

    def cli_verify_no_response(self, cli):
        """execute a CLI, asserting that the response is empty"""
        self.assert_equal(self.vapi.cli(cli), "", "CLI command response")

    def cli_verify_response(self, cli, expected):
        """execute a CLI, asserting that the response matches expectation"""
        try:
            reply = self.vapi.cli(cli)
        except CliFailedCommandError as cli_error:
            reply = str(cli_error)
        self.assert_equal(reply.strip(), expected, "CLI command response")

    def create_packet(self, test_case, sport = 57):
        """create a packet"""
        packet = (
            Ether(
                src=self.pg0.remote_mac, dst=self.pg0.local_mac
            )
            / IP(src=self.pg0.remote_ip4, dst=self.pg1.remote_ip4, ttl=255)
            / UDP(dport=test_case, sport=sport)
        )
        return packet

    def test_drop_cli(self):
        """Drop with feature enabled via CLI"""
        err = self.statistics.get_err_counter("/err/test/Drop")
        self.cli_verify_no_response(f"rust-test node {self.pg0.name}")

        packet = self.create_packet(1)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counter to have been incremented by one
        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err + 1)

        # Now disable and expect the packet counter to not be incremented
        err = new_err
        self.cli_verify_no_response(f"rust-test node {self.pg0.name} disable")

        packet = self.create_packet(1)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err)

    def enable_disable_api(self, sw_if_index, enable, node_type = "x1"):

        test_node_type = VppEnum.vl_api_test_node_type_t
        if node_type == "x1":
            api_node_type = test_node_type.TEST_NODE_TYPE_X1
        elif node_type == "x4":
            api_node_type = test_node_type.TEST_NODE_TYPE_X4
        else:
            raise Exception(f"Invalid node type: {node_type}")
        self.vapi.api(
            self.vapi.papi.test_enable_disable,
            {
                'sw_if_index': sw_if_index,
                'enable': enable,
                'node_type': api_node_type,
            },
        )

    def barrier_rw_lock(self, enable):
        self.vapi.api(
            self.vapi.papi.test_barrier_rw_lock,
            {
                'enable': enable,
            },
        )

    def process_node(self, dest):
        self.vapi.api(
            self.vapi.papi.test_process_node,
            {
                'enable': dest is not None,
                'dest': inet_pton(AF_INET, dest) if dest is not None else bytes([0, 0, 0, 0])
            },
        )

    def test_drop_api(self):
        """Drop with feature enabled via API"""
        err = self.statistics.get_err_counter("/err/test/Drop")
        self.enable_disable_api(self.pg0.sw_if_index, True)

        packet = self.create_packet(1)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counter to have been incremented by one
        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err + 1)

        # Now disable and expect the packet counter to not be incremented
        err = new_err
        self.enable_disable_api(self.pg0.sw_if_index, False)

        packet = self.create_packet(1)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err)

    def test_next_feature(self):
        """Forward to next feature"""
        self.enable_disable_api(self.pg0.sw_if_index, True)

        packet = self.create_packet(2)
        self.logger.info(ppp("Sending packet:", packet))
        self.send_and_expect(self.pg0, packet, self.pg1)

        self.logger.debug(self.vapi.cli("show trace"))

        # Clean up
        self.enable_disable_api(self.pg0.sw_if_index, False)

    def test_drop_manual_counter(self):
        """Drop with drop counter manual increment"""
        err = self.statistics.get_err_counter("/err/test/Drop")
        self.enable_disable_api(self.pg0.sw_if_index, True)

        packet = self.create_packet(3)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counters to have been incremented by one
        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err + 1)

        # Clean up
        self.enable_disable_api(self.pg0.sw_if_index, False)

    def test_drop_using_runtime_data_cache(self):
        """Drop using runtime data cache"""
        err = self.statistics.get_err_counter("/err/test/Drop")
        self.enable_disable_api(self.pg0.sw_if_index, True)

        # Send two packets, one to prime the cache and the second to use the cache
        packet = self.create_packet(4)
        self.logger.info(ppp("Sending two packets of:", packet))
        self.pg0.add_stream([packet, packet])
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counters to have been incremented by two
        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err + 2)

        # Clean up
        self.enable_disable_api(self.pg0.sw_if_index, False)

    def test_node_simple_counter(self):
        """Feature node incrementing simple counter"""
        simple_count = self.statistics.get_counter("/net/test/simple")[0][0]
        self.enable_disable_api(self.pg0.sw_if_index, True)

        packet = self.create_packet(5)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counter to have been incremented by one
        new_simple_count = self.statistics.get_counter("/net/test/simple")[0][0]
        self.assertEqual(new_simple_count, simple_count + 1)

        # Clean up
        self.enable_disable_api(self.pg0.sw_if_index, False)

    def test_node_combined_counter(self):
        """Feature node incrementing combined counter"""
        combined_count = self.statistics.get_counter("/net/test/combined")[0]
        self.enable_disable_api(self.pg0.sw_if_index, True)

        packet = self.create_packet(6)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        self.logger.debug(self.vapi.cli("show buffers"))

        # We only count the layer-3 packet size from an ip4-input feature
        packet_len = len(packet[IP])
        # Expect the combined counter to have been incremented by one packet and its size in bytes
        new_combined_count = self.statistics.get_counter("/net/test/combined")[0]
        self.assertEqual(new_combined_count[0]["packets"], combined_count[0]["packets"] + 1)
        self.assertEqual(new_combined_count[0]["bytes"], combined_count[0]["bytes"] + packet_len)

        # Now send a large packet that results in a chained buffer
        combined_count = new_combined_count
        packet = self.create_packet(6) / (3000 * "0")
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # We only count the layer-3 packet size from an ip4-input feature
        packet_len = len(packet[IP])
        # Expect the combined counter to have been incremented by one packet and its size in bytes
        new_combined_count = self.statistics.get_counter("/net/test/combined")[0]
        self.assertEqual(new_combined_count[0]["packets"], combined_count[0]["packets"] + 1)
        self.assertEqual(new_combined_count[0]["bytes"], combined_count[0]["bytes"] + packet_len)

        # Clean up
        self.enable_disable_api(self.pg0.sw_if_index, False)

    def test_barrier_rw_lock(self):
        """Make use of the barrier read/write lock"""
        err = self.statistics.get_err_counter("/err/test/Drop")
        self.enable_disable_api(self.pg0.sw_if_index, True)
        self.barrier_rw_lock(True)

        # Send a packet matching the source port policy
        packet = self.create_packet(7, sport=7)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counters to have been incremented by one
        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err + 1)
        err = new_err

        # Send a packet not matching the source port policy
        packet = self.create_packet(7)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counters to be unchanged
        new_err = self.statistics.get_err_counter("/err/test/Drop")
        self.assertEqual(new_err, err)

        # Clean up
        self.barrier_rw_lock(False)
        self.enable_disable_api(self.pg0.sw_if_index, False)

    def test_node_x4(self):
        """Node processing 4 buffers at a time"""
        err = self.statistics.get_err_counter("/err/testx4/Drop")
        self.enable_disable_api(self.pg0.sw_if_index, True, node_type="x4")

        packet = self.create_packet(1)
        self.logger.info(ppp("Sending packet:", packet))
        # Send full frame of 256 packets to validate that corner case
        self.pg0.add_stream(256 * packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counter to have been incremented by 256
        new_err = self.statistics.get_err_counter("/err/testx4/Drop")
        self.assertEqual(new_err, err + 256)

        # Now send frame of just one packet to validate that corner case
        err = new_err
        self.pg0.add_stream(packet)
        self.pg_start()

        # Expect the packet counter to have been incremented by 1
        new_err = self.statistics.get_err_counter("/err/testx4/Drop")
        self.assertEqual(new_err, err + 1)

        # Now disable and expect the packet counter to not be incremented
        err = new_err
        self.enable_disable_api(self.pg0.sw_if_index, False, node_type="x4")

        packet = self.create_packet(1)
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        new_err = self.statistics.get_err_counter("/err/testx4/Drop")
        self.assertEqual(new_err, err)

    def test_node_x4_next_feature(self):
        """Node processing 4 buffers at a time, forward to next feature"""
        self.enable_disable_api(self.pg0.sw_if_index, True, node_type="x4")

        packet = self.create_packet(2)
        self.logger.info(ppp("Sending packet:", packet))
        # Send full frame of 256 packets to validate that corner case
        self.send_and_expect(self.pg0, 256 * packet, self.pg1)

        # Now send frame of just one packet to validate that corner case
        self.send_and_expect(self.pg0, packet, self.pg1)

        # Send a frame of 7 packets to validate no undefined behaviour for greater than 4, but a
        # not a multiple of 8
        self.send_and_expect(self.pg0, 7 * packet, self.pg1)

        self.logger.debug(self.vapi.cli("show trace"))

        # Clean up
        self.enable_disable_api(self.pg0.sw_if_index, False, node_type="x4")

    def test_node_x4_mixed_next(self):
        """Node processing 4 buffers at a time, mixed next nodes"""
        err = self.statistics.get_err_counter("/err/testx4/Drop")
        self.enable_disable_api(self.pg0.sw_if_index, True, node_type="x4")

        packet1 = self.create_packet(1)
        packet2 = self.create_packet(2)
        # Send frame of 4 packets resulting in interleaved mixed next nodes
        self.pg0.add_stream([packet1, packet2, packet1, packet2])
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counter to have been incremented by 2
        new_err = self.statistics.get_err_counter("/err/testx4/Drop")
        self.assertEqual(new_err, err + 2)

        # Clean up
        self.enable_disable_api(self.pg0.sw_if_index, False, node_type="x4")

    def test_node_x4_mixed_feature_chains(self):
        """Node processing 4 buffers at a time, buffers with different feature chains"""
        # testx4 is enabled on both interfaces but testchain, which runs after it, only on
        # pg0, so one ip4-unicast frame holding packets from both reaches testx4 with two
        # different config strings. Every packet must reach testchain only if it came from
        # pg0, at most once, and leave by its own route, however the frame splits into strides
        # of four; testx4 dropping some (UDP dport 1) mixes next nodes within a stride too.
        self.enable_disable_api(self.pg0.sw_if_index, True, node_type="x4")
        self.enable_disable_api(self.pg1.sw_if_index, True, node_type="x4")
        self.cli_verify_no_response(f"rust-test chain {self.pg0.name}")
        try:
            for n0, n1 in [
                (1, 1), (1, 3), (2, 2), (3, 1), (3, 5), (4, 4),
                (5, 7), (6, 2), (9, 13), (100, 155),
            ]:
                for drop_every in (0, 3):
                    with self.subTest(n0=n0, n1=n1, drop_every=drop_every):
                        self.check_mixed_feature_chains(n0, n1, drop_every)
        finally:
            self.cli_verify_no_response(f"rust-test chain {self.pg0.name} disable")
            self.enable_disable_api(self.pg1.sw_if_index, False, node_type="x4")
            self.enable_disable_api(self.pg0.sw_if_index, False, node_type="x4")

    def check_mixed_feature_chains(self, n0, n1, drop_every):
        enabled = self.statistics.get_err_counter("/err/testchain/Enabled")
        foreign = self.statistics.get_err_counter("/err/testchain/Foreign")

        def dport(i):
            return 1 if drop_every and i % drop_every == 0 else 2

        from_pg0 = [self.create_packet(dport(i), sport=1000 + i) for i in range(n0)]
        from_pg1 = [
            Ether(src=self.pg1.remote_mac, dst=self.pg1.local_mac)
            / IP(src=self.pg1.remote_ip4, dst=self.pg0.remote_ip4, ttl=255)
            / UDP(dport=2, sport=2000 + i)
            for i in range(n1)
        ]
        forwarded_from_pg0 = [1000 + i for i in range(n0) if dport(i) == 2]

        self.pg0.add_stream(from_pg0)
        self.pg1.add_stream(from_pg1)
        self.pg_enable_capture(self.pg_interfaces)
        self.pg_start()

        # Exact counts and identities: nothing dropped, duplicated or sent the wrong way.
        if forwarded_from_pg0:
            out_pg1 = self.pg1.get_capture(len(forwarded_from_pg0))
            self.assertEqual(sorted(p[UDP].sport for p in out_pg1), forwarded_from_pg0)
        else:
            self.pg1.assert_nothing_captured()
        out_pg0 = self.pg0.get_capture(n1)
        self.assertEqual(sorted(p[UDP].sport for p in out_pg0), list(range(2000, 2000 + n1)))

        # testchain saw each forwarded pg0 packet exactly once and nothing from pg1.
        self.assertEqual(
            self.statistics.get_err_counter("/err/testchain/Enabled") - enabled,
            len(forwarded_from_pg0),
        )
        self.assertEqual(
            self.statistics.get_err_counter("/err/testchain/Foreign") - foreign, 0
        )
    def test_version_required(self):
        """Plugins requiring another VPP build are refused"""
        plugins = self.vapi.cli("show plugins")
        # test_plugin.so requires the build it was compiled against (vpp_plugin::VPP_BUILD_VER),
        # i.e. this one, so it loaded.
        self.assertIn("test_plugin.so", plugins)
        # version_mismatch_plugin.so requires a build that does not exist, so VPP refused it:
        # neither the plugin nor its CLI command is present.
        self.assertNotIn("version_mismatch_plugin.so", plugins)
        with self.assertRaises(CliFailedCommandError):
            self.vapi.cli("rust-test version-mismatch")

    def test_process_node(self):
        """Use a process node"""
        self.process_node(self.pg1.remote_ip4)

        self.pg1.enable_capture()
        self.pg1.wait_for_packet(2)

        # Clean up
        self.process_node(None)

    def test_vnet_error(self):
        """VNET error being generated and returned from a CLI command"""
        self.cli_verify_response(f"rust-test negative vnet-error", "rust-test negative: Invalid value (Test)")

    def test_message(self):
        """Messages"""
        self.cli_verify_no_response("rust-test message")

    def test_counters(self):
        """Counters"""
        self.cli_verify_no_response(f"rust-test counter simple")
        self.cli_verify_no_response(f"rust-test counter combined")

    def test_type_in_message(self):
        """API with type nested in message"""
        self.vapi.api(
            self.vapi.papi.test_type_in_message,
            {
                'test_type': {
                    'field1': 42,
                },
            },
        )

    def test_array_in_message(self):
        """API with arrays in message"""
        test_node_type = VppEnum.vl_api_test_node_type_t
        self.vapi.api(
            self.vapi.papi.test_array,
            {
                'array1': [42, 0xdeadbeef, 0, 0xffffffff],
                'array2': {
                    'array': [42, 0xffff],
                },
                'array3': [test_node_type.TEST_NODE_TYPE_X1, test_node_type.TEST_NODE_TYPE_X4],
                'array4': [0.0, 42.0],
            },
        )

    def test_api_reply(self):
        """API reply"""
        reply = self.vapi.api(
            self.vapi.papi.test_response,
            {
                'value': 42,
            },
        )
        self.assertEqual(reply.value, 42, f"Expected value of 42 to be returned but got {reply.value}")

    def test_api_reply_no_retval(self):
        """API reply with no retval field"""
        reply = self.vapi.api(
            self.vapi.papi.test_response_no_retval,
            {
                'value': 42.0,
            },
        )
        self.assertEqual(reply.value, 42.0, f"Expected value of 42.0 to be returned but got {reply.value}")

    def test_api_stream(self):
        """API using legacy streams"""
        details = self.vapi.papi.test_dump()
        self.assertEqual(len(details), 2, f"Expected 2 details to be returned but got {len(details)}")
        self.assertEqual(details[0].value, 1, f"Expected first details value to be 1 but got {details[0].value}")
        self.assertEqual(details[1].value, 2, f"Expected second details value to be 2 but got {details[1].value}")

    def test_api_stream_message(self):
        """API using modern streams"""
        _, details = self.vapi.papi.test_stream_get()
        self.assertEqual(len(details), 2, f"Expected 2 details to be returned but got {len(details)}")
        self.assertEqual(details[0].value, 1, f"Expected first details value to be 1 but got {details[0].value}")
        self.assertEqual(details[1].value, 2, f"Expected second details value to be 2 but got {details[1].value}")

    def test_api_typedef(self):
        """API using typedef"""
        self.vapi.api(
            self.vapi.papi.test_typedef,
            {
                'addr': bytes([1, 2, 3, 4]),
            },
        )

    def test_api_union(self):
        """API using union"""
        test_address_family = VppEnum.vl_api_test_address_family_t
        self.vapi.api(
            self.vapi.papi.test_union,
            {
                'addr': {
                    'af': test_address_family.TEST_ADDRESS_IP4,
                    'un': {
                        'ip4': bytes([1, 2, 3, 4]),
                    }
                }
            },
        )
        self.vapi.api(
            self.vapi.papi.test_union,
            {
                'addr': {
                    'af': test_address_family.TEST_ADDRESS_IP6,
                    'un': {
                        'ip6': bytes([1, 2, 3, 4, 5, 6, 7, 8, 9, 0xa, 0xb, 0xc, 0xd, 0xe, 0xf, 0]),
                    }
                }
            },
        )

    def test_api_vla(self):
        """API using variable length arrays"""
        reply = self.vapi.api(
            self.vapi.papi.test_variable_array_u32,
            {
                'nitems': 4,
                'values': [42, 0xdeadbeef, 0, 0xffffffff],
            },
        )
        self.assertEqual(reply.values, [42, 0xdeadbeef, 0, 0xffffffff])
        self.vapi.api(
            self.vapi.papi.test_variable_array_u8,
            {
                'nitems': 3,
                'values': bytes([42, 0, 0xff]),
            },
        )
        self.vapi.api(
            self.vapi.papi.test_variable_array_f64,
            {
                'nitems': 2,
                'values': [0.0, 42.0],
            },
        )
        test_node_type = VppEnum.vl_api_test_node_type_t
        self.vapi.api(
            self.vapi.papi.test_variable_array_custom,
            {
                'nitems': 2,
                'values': [test_node_type.TEST_NODE_TYPE_X1, test_node_type.TEST_NODE_TYPE_X4],
            },
        )
        self.vapi.api(
            self.vapi.papi.test_variable_array_in_type,
            {
                'field': {
                    'nitems': 4,
                    'values': [42, 0xdead, 0, 0xffff],
                }
            },
        )

    def test_api_string(self):
        """API using strings"""
        reply = self.vapi.api(
            self.vapi.papi.test_string,
            {
                'fixed': 'Hello World!'.ljust(63),
                'variable': 'Hello World!',
            },
        )
        self.assertEqual(reply.fixed, "Goodbye World!".ljust(64))
        self.assertEqual(reply.variable, "Goodbye World!")

    def test_api_enumflag(self):
        """API using enumflag"""
        test_dir = VppEnum.vl_api_test_dir_t
        test_dir2 = VppEnum.vl_api_test_dir2_t
        self.vapi.api(
            self.vapi.papi.test_enumflag,
            {
                'flags': test_dir.TEST_DIR_RX | test_dir.TEST_DIR_TX,
                'flags2': test_dir2.TEST_DIR2_RX | test_dir2.TEST_DIR2_TX,
            },
        )

if __name__ == "__main__":
    unittest.main(testRunner=VppTestRunner)
