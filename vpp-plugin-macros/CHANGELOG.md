# 0.3.0 (24th October 2026)

### Added

 * Register VPP's baseline node function variant
 * Support version_required in vlib_plugin_register

### Changed

 * Change supported VPP version to 26.06

# 0.2.1 (30th May 2026)

### Added

* Add support for process nodes

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
