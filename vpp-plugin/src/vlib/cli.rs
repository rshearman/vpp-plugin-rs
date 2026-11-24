//! Command line interface

use std::{cell::UnsafeCell, fmt, io::Write, marker::PhantomData};

use crate::{
    bindings::{
        uword, vlib_cli_command_t, vlib_cli_output_function_t, vlib_helper_get_current_process,
        vlib_helper_get_global_main, vlib_helper_remove_cli_command, vlib_main_t,
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

/// Returns the `is_mp_safe` value for a CLI command function taking a [`CliOutput`]
///
/// As per [`cli_is_mp_safe`], but for functions taking the optional third argument.
#[doc(hidden)]
pub const fn cli_is_mp_safe_with_output<M: CliMainRef>(
    _function: fn(&mut M, &str, &mut CliOutput) -> Result<(), ErrorStack>,
) -> uword {
    M::IS_MP_SAFE
}

/// Output of a CLI command
///
/// This is passed to CLI command functions that take it as an optional third argument (see
/// [`crate::vlib_cli_command`]), and writes to wherever the command was run from, e.g. the
/// interactive CLI or the reply to a `cli_inband` API message. Use it with [`write!`] and
/// [`writeln!`], which don't return a `Result` since writing can't fail. It also implements
/// [`fmt::Write`] for use with code that is generic over the writer.
///
/// Each complete line is written to VPP as soon as it is written to the `CliOutput`, so output
/// streams to an interactive CLI as the command runs. A partial line is held until it is
/// completed by a newline, since VPP treats each write as one or more complete lines (e.g. for
/// paging output), and if the command returns while a partial line is held, it is written with
/// a newline added, as per `vlib_cli_output`.
///
/// # Examples
///
/// ```
/// # use vpp_plugin::{vlib_cli_command, vlib::{self, cli::CliOutput}, vppinfra::error::ErrorStack};
/// #[vlib_cli_command(path = "show rust-example", short_help = "show rust-example")]
/// fn show_command(
///     _vm: &mut vlib::MainRef,
///     _input: &str,
///     output: &mut CliOutput,
/// ) -> Result<(), ErrorStack> {
///     for i in 0..3 {
///         writeln!(output, "Entry {i}");
///     }
///     Ok(())
/// }
/// ```
pub struct CliOutput {
    function: vlib_cli_output_function_t,
    function_arg: uword,
    partial_line: std::vec::Vec<u8>,
    // The output function and its argument are tied to the main thread
    _not_send: PhantomData<*mut ()>,
}

impl CliOutput {
    /// Creates a `CliOutput` for the current process of a `vlib_main_t`
    ///
    /// Used by the [`crate::vlib_cli_command`] macro.
    ///
    /// # Safety
    ///
    /// - The pointer must be the `vlib_main_t` that VPP passed to a CLI command function.
    /// - The `CliOutput` must not be used after the CLI command function returns, since the
    ///   output function's argument may refer to state that only lives as long as the call.
    #[doc(hidden)]
    pub unsafe fn from_ptr(vm: *mut vlib_main_t) -> Self {
        // SAFETY: the caller ensures that `vm` is valid, and the returned process is either
        // null, when not in process context, or valid.
        let (function, function_arg) = unsafe {
            let cp = vlib_helper_get_current_process(vm);
            if cp.is_null() {
                (None, 0)
            } else {
                ((*cp).output_function, (*cp).output_function_arg)
            }
        };
        Self {
            function,
            function_arg,
            partial_line: std::vec::Vec::new(),
            _not_send: PhantomData,
        }
    }

    /// Writes a string
    ///
    /// Complete lines are written to VPP immediately, and any partial line at the end is held
    /// until it is completed.
    pub fn write_str(&mut self, s: &str) {
        let bytes = s.as_bytes();
        let Some(last_newline) = bytes.iter().rposition(|&b| b == b'\n') else {
            self.partial_line.extend_from_slice(bytes);
            return;
        };
        let (lines, rest) = bytes.split_at(last_newline + 1);
        if self.partial_line.is_empty() {
            self.output(lines);
        } else {
            self.partial_line.extend_from_slice(lines);
            self.output(&self.partial_line);
            self.partial_line.clear();
        }
        self.partial_line.extend_from_slice(rest);
    }

    /// Writes formatted output
    ///
    /// This is used by [`write!`] and [`writeln!`], and takes precedence over
    /// [`fmt::Write::write_fmt`] so that they don't return a `Result`. If a formatting trait
    /// implementation returns an error, the output up to that point is still written.
    pub fn write_fmt(&mut self, args: fmt::Arguments<'_>) {
        let _ = fmt::Write::write_fmt(self, args);
    }

    /// Writes bytes, which must be empty or end with a newline, to the output
    fn output(&self, bytes: &[u8]) {
        debug_assert!(bytes.last().is_none_or(|&b| b == b'\n'));
        if bytes.is_empty() {
            return;
        }
        match self.function {
            // SAFETY: the output function and its argument came from the current process for
            // this CLI command, the command hasn't returned yet (per the requirements of
            // `from_ptr`), and VPP's output functions only read the buffer.
            Some(function) => unsafe {
                function(
                    self.function_arg,
                    bytes.as_ptr() as *mut u8,
                    bytes.len() as uword,
                )
            },
            // As per vlib_cli_output, which writes to stdout when there's no output function.
            // Errors are ignored as they are there.
            None => {
                let mut stdout = std::io::stdout().lock();
                let _ = stdout.write_all(bytes);
                let _ = stdout.flush();
            }
        }
    }
}

impl fmt::Write for CliOutput {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        CliOutput::write_str(self, s);
        Ok(())
    }
}

impl Drop for CliOutput {
    fn drop(&mut self) {
        if !self.partial_line.is_empty() {
            self.partial_line.push(b'\n');
            self.output(&self.partial_line);
        }
    }
}

impl fmt::Debug for CliOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CliOutput")
            .field("partial_line", &String::from_utf8_lossy(&self.partial_line))
            .finish_non_exhaustive()
    }
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
    use std::{cell::RefCell, fmt::Write, marker::PhantomData};

    use crate::{
        bindings::{uword, vlib_cli_command_t},
        vlib::cli::{CliOutput, CommandRegistration},
    };

    /// Records each call to the output function
    type Writes = RefCell<Vec<String>>;

    unsafe extern "C" fn record_output(arg: uword, buffer: *mut u8, buffer_bytes: uword) {
        // SAFETY: arg is a pointer to `Writes` and buffer is valid for buffer_bytes, as passed
        // by CliOutput
        unsafe {
            let writes = &*(arg as *const Writes);
            let bytes = std::slice::from_raw_parts(buffer, buffer_bytes as usize);
            writes
                .borrow_mut()
                .push(String::from_utf8(bytes.to_vec()).unwrap());
        }
    }

    /// Calls `f` with a `CliOutput` recording to the returned writes, after the output is dropped
    fn record(f: impl FnOnce(&mut CliOutput, &Writes)) -> Vec<String> {
        let writes = Writes::default();
        let mut output = CliOutput {
            function: Some(record_output),
            function_arg: &writes as *const Writes as uword,
            partial_line: Vec::new(),
            _not_send: PhantomData,
        };
        f(&mut output, &writes);
        drop(output);
        writes.into_inner()
    }

    #[test]
    fn cli_output_streams_complete_lines() {
        let writes = record(|output, writes| {
            writeln!(output, "line 1");
            assert_eq!(*writes.borrow(), ["line 1\n"]);
            write!(output, "line 2\nline 3\n");
            assert_eq!(*writes.borrow(), ["line 1\n", "line 2\nline 3\n"]);
        });
        assert_eq!(writes, ["line 1\n", "line 2\nline 3\n"]);
    }

    #[test]
    fn cli_output_holds_partial_lines() {
        let writes = record(|output, writes| {
            write!(output, "a");
            write!(output, "{}", 1);
            assert!(writes.borrow().is_empty());
            write!(output, "b\nc\nd");
            assert_eq!(*writes.borrow(), ["a1b\nc\n"]);
            writeln!(output);
            assert_eq!(*writes.borrow(), ["a1b\nc\n", "d\n"]);
        });
        assert_eq!(writes, ["a1b\nc\n", "d\n"]);
    }

    #[test]
    fn cli_output_terminates_partial_line_on_drop() {
        let writes = record(|output, _| {
            write!(output, "x\ny");
        });
        assert_eq!(writes, ["x\n", "y\n"]);
    }

    #[test]
    fn cli_output_empty() {
        let writes = record(|output, _| {
            write!(output, "");
            writeln!(output);
        });
        assert_eq!(writes, ["\n"]);
        assert!(record(|_, _| {}).is_empty());
    }

    #[test]
    fn cli_output_fmt_write() {
        fn generic(w: &mut impl Write) -> std::fmt::Result {
            write!(w, "generic ")?;
            writeln!(w, "{}", 42)
        }
        let writes = record(|output, _| generic(output).unwrap());
        assert_eq!(writes, ["generic 42\n"]);
    }

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
