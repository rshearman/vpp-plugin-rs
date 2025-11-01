#!/usr/bin/env python3
""" BFD tests """

from __future__ import division

import unittest

from framework import VppTestCase
from asfframework import (
    tag_run_solo,
    VppTestRunner,
)
from vpp_ip_route import VppIpRoute, VppRoutePath


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


if __name__ == "__main__":
    unittest.main(testRunner=VppTestRunner)
