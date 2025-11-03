#!/usr/bin/env python3
""" BFD tests """

from __future__ import division

import unittest

from scapy.layers.inet import IP
from scapy.layers.l2 import Ether
from scapy.layers.inet import ICMP

from framework import VppTestCase
from asfframework import (
    tag_run_solo,
    VppTestRunner,
)
from util import ppp
from vpp_ip_route import VppIpRoute, VppRoutePath
from vpp_papi_provider import CliFailedCommandError


def set_ipv4_pfx_route_info(cls, pg_if, dst_ip, set_src):
    paths = []
    # Get list of all the next hops
    for nh_host in pg_if.remote_hosts:
        nh_host_ip = nh_host.ip4
        paths.append(VppRoutePath(nh_host_ip, pg_if.sw_if_index))
    cls.dst_ip_net = dst_ip
    # Create a route pointing to list of next hops
    if paths:
        rip = VppIpRoute(cls, cls.dst_ip_net, 32, paths)
        rip.add_vpp_config()
        cls.logger.info("Route via %s on %s created" % (paths, pg_if.name))


@tag_run_solo
class ExampleTestCase(VppTestCase):
    """Example Node"""

    pg0 = None

    @classmethod
    def setUpClass(cls):
        super(ExampleTestCase, cls).setUpClass()
        cls.__doc__ = (
            f"""""Example Node"""
        )
        try:
            cls.create_pg_interfaces([0])
            cls.pg0.config_ip4()
            cls.pg0.config_ip6()
            cls.pg0.configure_ipv4_neighbors()
            cls.pg0.admin_up()
            cls.pg0.resolve_arp()
            cls.pg0.resolve_ndp()

        except Exception:
            super(ExampleTestCase, cls).tearDownClass()
            raise

    @classmethod
    def tearDownClass(cls):
        super(ExampleTestCase, cls).tearDownClass()

    def setUp(self):
        super(ExampleTestCase, self).setUp()
        self.pg0.enable_capture()

    def tearDown(self):
        self.vapi.collect_events()  # clear the event queue
        super(ExampleTestCase, self).tearDown()

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

    def create_packet(self):
        """create a packet"""
        packet = (
            Ether(
                src=self.pg0.remote_mac, dst=self.pg0.local_mac
            )
            / IP(src=self.pg0.remote_ip4, dst=self.pg0.local_ip4, ttl=255)
            / ICMP()
        )
        return packet

    def test_example_cli(self):
        """Example node via CLI"""
        err = self.statistics.get_err_counter("/err/example/Drop")
        self.cli_verify_no_response(f"rust-example {self.pg0.name}")

        packet = self.create_packet()
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        self.logger.debug(self.vapi.cli("show trace"))

        # Expect the packet counter to have been incremented by one
        new_err = self.statistics.get_err_counter("/err/example/Drop")
        self.assertEqual(new_err, err + 1)

        # Now disable and expect the packet counter to not be incremented
        err = new_err
        self.cli_verify_no_response(f"rust-example {self.pg0.name} disable")

        packet = self.create_packet()
        self.logger.info(ppp("Sending packet:", packet))
        self.pg0.add_stream(packet)
        self.pg_start()

        new_err = self.statistics.get_err_counter("/err/example/Drop")
        self.assertEqual(new_err, err)

if __name__ == "__main__":
    unittest.main(testRunner=VppTestRunner)
