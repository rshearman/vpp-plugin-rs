# VPP plugin

A framework for writing high-performance, reliable VPP plugins in Rust.

## Overview

[VPP](https://wiki.fd.io/view/VPP/What_is_VPP%3F) is a high performance packet processing
with support for a large number of features, and enables writing plugins to extend its
functionality.

The framework here allows writing VPP plugins in Rust. It includes
bindings to the VPP C API, abstractions for writing VPP nodes, and a parser/generator for `.api` files, with the overall goal of
having performance parity with C plugins for fast path packet processing.

In line with the Rust's soundness guarantee, safe Rust code using this framework should not cause undefined behaviour. Although this gurantee can be broken when unsafe code is used (which is required when implementing a feature plugin), the goal is to simplify the conditions that need to be upheld versus a C implementation and so making it more likely that the code functions correctly on the first try.

## Example

See vpp-example-plugin for how to build a plugin, including the `Cargo.toml` contents.

## Caveats

At the moment, interface and crypto engine plugins are not supported, only plugins for nodes.

Patches are required to be applied to VPP in order to build and use VPP plugins built in Rust. See the `patches` directory for details.

## Short term goals

- Implement known unimplemented items in API code generator
- Counters
- Process nodes using async Rust

## Longer term goals

- Replace Python tests with tests that use VPP client Rust crates (https://github.com/ayourtch/vpp-api)
