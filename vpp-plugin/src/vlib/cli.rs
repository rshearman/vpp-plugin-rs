//! Command line interface

use std::cell::UnsafeCell;

use crate::{
    bindings::{
        uword, vlib_cli_command_t, vlib_helper_get_global_main, vlib_helper_remove_cli_command,
        vlib_main_t,
    },
    vlib::{BarrierHeldMainRef, MainRef},
    vppinfra::error::ErrorStack,
};

mod sealed {
    pub trait Sealed {}

    impl Sealed for crate::vlib::MainRef {}
    impl Sealed for crate::vlib::BarrierHeldMainRef {}
}

/// Main context type that a CLI command function can take as its first argument
///
/// This is used by the [`crate::vlib_cli_command`] macro to work out whether the CLI command
/// is MP-safe, based on the type of the function's first argument:
///
/// - [`BarrierHeldMainRef`]: the command is not MP-safe, so VPP takes the main thread barrier
///   before calling it.
/// - [`MainRef`]: the command is MP-safe, so VPP calls it without taking the barrier. The
///   command may still take the barrier itself for part of its work, using
///   [`MainRef::barrier_sync`].
///
/// This trait is sealed and cannot be implemented outside of this crate.
///
/// # Safety
///
/// Implementors must be `repr(transparent)` wrappers over `vlib_main_t` and `IS_MP_SAFE` must
/// only be non-zero if the type doesn't imply that the main thread barrier is held.
#[diagnostic::on_unimplemented(
    message = "`{Self}` cannot be the main context argument of a CLI command function",
    note = "use `&mut vlib::MainRef` for an MP-safe command, or `&mut vlib::BarrierHeldMainRef` for VPP to take the barrier before calling the command"
)]
pub unsafe trait CliMainRef: sealed::Sealed {
    /// Value for the `is_mp_safe` field of `vlib_cli_command_t`
    const IS_MP_SAFE: uword;

    /// Creates a `&mut Self` directly from a pointer
    ///
    /// # Safety
    ///
    /// - The pointer must be the `vlib_main_t` that VPP passed to a CLI command function whose
    ///   `is_mp_safe` field was set to [`Self::IS_MP_SAFE`].
    /// - The returned reference must not outlive the call to the CLI command function.
    unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_main_t) -> &'a mut Self;
}

// SAFETY: `MainRef` is a `repr(transparent)` wrapper over `vlib_main_t` and doesn't imply the
// barrier is held.
unsafe impl CliMainRef for MainRef {
    const IS_MP_SAFE: uword = 1;

    unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_main_t) -> &'a mut Self {
        // SAFETY: VPP passes a valid `vlib_main_t` for the main thread to CLI command functions,
        // and the caller ensures the reference doesn't outlive the call.
        unsafe { MainRef::from_ptr_mut(ptr) }
    }
}

// SAFETY: `BarrierHeldMainRef` is a `repr(transparent)` wrapper over `vlib_main_t` and
// `IS_MP_SAFE` is zero.
unsafe impl CliMainRef for BarrierHeldMainRef {
    const IS_MP_SAFE: uword = 0;

    unsafe fn from_ptr_mut<'a>(ptr: *mut vlib_main_t) -> &'a mut Self {
        // SAFETY: VPP passes a valid `vlib_main_t` for the main thread to CLI command functions,
        // takes the barrier before calling non-MP-safe CLI commands, and the caller ensures the
        // reference doesn't outlive the call.
        unsafe { BarrierHeldMainRef::from_ptr_mut(ptr) }
    }
}

/// Returns the `is_mp_safe` value for a CLI command function
///
/// Used by the [`crate::vlib_cli_command`] macro. Taking a function pointer both infers `M`
/// from the function's first argument and checks that the function has the expected signature.
#[doc(hidden)]
pub const fn cli_is_mp_safe<M: CliMainRef>(
    _function: fn(&mut M, &str) -> Result<(), ErrorStack>,
) -> uword {
    M::IS_MP_SAFE
}

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

#[cfg(test)]
mod tests {
    use crate::{bindings::vlib_cli_command_t, vlib::cli::CommandRegistration};

    #[test]
    fn test_cmd_reg() {
        // CommandRegistration::new is a const function and Rust doesn't generate coverage data
        // when such functions are evaluated at compile time
        // (https://github.com/rust-lang/rust/issues/124732), so force it to be evaluated at
        // runtime.
        let command = std::hint::black_box(CommandRegistration::new(vlib_cli_command_t::default()));

        let command = Box::new(command);
        // Get a raw pointer so we can ignore the static lifetime requirement of register
        let command = Box::into_raw(command);

        // SAFETY: preconditions of register/unregister don't have to be met because we are
        // calling this outside of the VPP application, meaning that pointers in the
        // vlib_cli_command_t won't be dereferenced.
        unsafe {
            (*command).register();
            (*command).unregister();

            let _ = Box::from_raw(command);
        }
    }
}
