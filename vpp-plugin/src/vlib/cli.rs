//! Command line interface

use std::cell::UnsafeCell;

use crate::bindings::{
    vlib_cli_command_t, vlib_helper_get_global_main, vlib_helper_remove_cli_command,
};

/// Registration for CLI command
///
/// This is typically created automatically using the [`crate::vlib_cli_command`] macro.
pub struct CommandRegistration(UnsafeCell<vlib_cli_command_t>);

impl CommandRegistration {
    /// Creates a new `CommandRegistration` from the given registration data
    pub const fn new(command: vlib_cli_command_t) -> Self {
        Self(UnsafeCell::new(command))
    }

    /// Registers the CLI command with VPP
    ///
    /// # Safety
    ///
    /// - Must be called only once for this node registration.
    /// - Must be called from a constructor function that is invoked before VPP initialises.
    /// - The following pointers in the registration data must be valid:
    ///   - `path` (must be a valid, nul-terminated string)
    ///   - `short_help` (must be a valid, nul-terminated string or null)
    ///   - `long_help` (must be a valid, nul-terminated string or null)
    /// - Other pointers must either point to a valid object of the appropriate type or be null.
    pub unsafe fn register(&'static self) {
        // SAFETY: preconditions of the method are met
        unsafe {
            let vgm = vlib_helper_get_global_main();
            let cm = &mut (*vgm).cli_main;
            let reg = self.0.get();
            (*reg).next_cli_command = cm.cli_command_registrations;
            cm.cli_command_registrations = reg;
        }
    }

    /// Unregisters the CLI command from VPP
    ///
    /// # Safety
    ///
    /// - Must be called only once for this registration.
    /// - Must be called from a destructor function that is invoked after VPP uninitialises.
    /// - The CLI command must have been previously registered with VPP using [`Self::register`].
    pub unsafe fn unregister(&self) {
        // SAFETY: preconditions of the method are met
        unsafe {
            let vgm = vlib_helper_get_global_main();
            let cm = &mut (*vgm).cli_main;
            vlib_helper_remove_cli_command(cm, self.0.get());
        }
    }
}

// SAFETY: there is nothing in vlib_cli_command that is tied to a specific thread or that
// mutates global state, so it's safe to send between threads.
unsafe impl Send for CommandRegistration {}
// SAFETY: CommandRegistration doesn't allow any modification after creation (and vpp doesn't
// modify it afterwards either), so it's safe to access from multiple threads. The only exception
// to this is the register/unregister methods, but it's the duty of the caller to ensure they are
// called at times when no other threads have a reference to the object.
unsafe impl Sync for CommandRegistration {}
