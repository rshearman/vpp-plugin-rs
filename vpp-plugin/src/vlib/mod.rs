//! VPP application library
//!
//! This module contains abstractions around VPP's application layer, `vlib`.

pub mod main;

pub use main::{BarrierHeldMainRef, MainRef};
