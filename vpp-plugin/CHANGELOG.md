# 0.3.0 (24th October 2026)

### Added

* vlib: main: support taking the barrier
* Export VPP_BUILD_VER

### Changed

* Change supported VPP version to 26.06
* vnet: types: simplify ip6_header_t Clone impl

### Fixed

* vlib: buffer: fix the documentation of current_ptr_mut
* vlib: main: make BarrierHeldMainRef repr(transparent)
* vlib: sync: fix lifetime of read and write method
* vnet: types: fix Display impls missing last character
* vnet: types: fix unsoundness in SwIfIndex Display and FromStr impls
* vnet: types: error on trailing input for SwIfIndex parsing
* node_generic: x4 feature-next shortcut must match config index, advance all four

# 0.2.2 (30th May 2026)

### Added

* vppinfra: add timer wheel implementation
* Add support for process nodes
* vnet: types: add SwIfIndex::LOCAL0 constant
* vnet: buffer: add tx/rx sw_if_index accessors
* vlib: buffer: support setting buffer flags
* vnet: buffer: allow converting from vnet buffer flags to vlib
* vlib: buffer: support appending data to buffers
* vlib: buffer: add BufferWithContext::into_parts()
* vpp-plugin: add vnet_helper_ip4_header_checksum
* vlib: node: support incrementing error counters for process nodes

### Changed

* vlib: bindings: update bindings with change to vpp 26.02
* vlib: node: relax constraints on constructing and using NodeRuntimeRef
* vlib: buffer: no longer mark buffer allocation types/methods as experimental

### Fixed

* vlib: sync: fix rustdoc::invalid-rust-codeblocks
* vlib: node: fix missing conditions in node registration safety doc
* vppinfra: fix renamed_and_removed_lints compiler warning
* vlib: buffer: fix undefined behaviour in get_buffers test
* buffer: add additional pre-condition in MainRef::get_buffers
* vppinfra: vec: fix UB in IntoIter<T>::clone
* vnet: buffer: improve vnet_get_config_data safety doc
* vlib: sync: improve Sync safety doc for BarrierRwLockWriteGuard

# 0.2.1 (12th April 2026)

### Added

* vpp-plugin: add a read/write lock built on VPP barrier semantics

### Changed

* vpp-plugin: refactor the generic_feature_node_x4
* vpp-plugin: refactor the generic_feature to use the assume_init_ref instead of transmute

### Fixed

* vnet/buffer: fix clippy lint
* vlib/node_generic: fix x4 prefetch bounds check

# 0.2.0 (24th January 2026)

### Added

* vlib: add support for simple counters
* counters: add support for combined counters
* buffer: add prefetching methods
* node: add NodeFlags type and NodeRuntime::flags()
* node_generic: add support for processing four buffers per iteration
* vpp-plugin-api-gen: implement writing of enums
* vnet-error: add error constants
* vec: implement FromIterator and IntoIterator for Vec
* vpp-plugin-api-gen: support generating code for fixed array fields
* vlibapi: add Stream struct
* vpp-plugin-api-gen: support stream messages
* vpp-plugin-api-gen: generate API enums as packed
* vlibapi: add Unaligned* types
* vpp-plugin-api-gen: add support for variable-length arrays in messages
* vpp-plugin-api-gen: add support for VLAs in typedef blocks
* vlibapi: add fixed and variable string types for use in API messages
* vpp-plugin-api-gen: support variable and fixed sized strings
* vpp-plugin-macros: include arm64 architecture variants
* vpp-plugin-api-gen: add support for generating enumflag types
* vpp-plugin-api-gen: support using imported types

### Changed

* vpp-plugin-api-gen: generate newtypes for aliases
* vpp-plugin-api-gen: make f64 endian swap explicitly no-op
* vlibapi: allow constructing dynamically-sized messages
* Change supported VPP version from 25.10 (with patches) to 26.02

### Fixed

* vpp-plugin-macros: fix hardcoded node name in vlib_node macro
* vpp-plugin-api-gen: fix unused *_calc_size function for typedefs
* vpp-plugin-api-gen: fix support for unions
* vppinfra: fix unused variable warning on CPUs other than x86-64
* buffer: improve docstrings
* vpp-plugin-api-gen: generate code that compiles with 2024 edition
* vpp-plugin-macros: vpp version robustness for vlib_plugin_register

# 0.1.0 (21st November 2025)

* Initial version
