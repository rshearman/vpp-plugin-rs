//! VPP error type abstractions
//!
//! This module provides abstractions around VPP's error handling, including
//! an error stack type that can represent a chain of errors similar to Rust's
//! [`std::error::Error`].
use core::str;
use std::{
    error::Error as StdError,
    ffi::{CStr, CString},
    fmt::{self, Display},
    mem::ManuallyDrop,
};

use super::{Vec, VecRef};
use crate::bindings::{_clib_error_return, clib_error_t, uword, CLIB_ERROR_ERRNO_VALID};

/// A single VPP error
///
/// This type represents a single error in VPP, corresponding to a `clib_error_t`.
#[derive(Debug, Copy, Clone)]
#[repr(transparent)]
pub struct Error(clib_error_t);

impl fmt::Display for Error {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.0.what.is_null() {
            return write!(fmt, "<none>");
        }

        if !self.0.where_.is_null() {
            // SAFETY: where_ is a valid non-null C string
            let where_c = unsafe { CStr::from_ptr(self.0.where_.cast()) };
            write!(fmt, "{}: ", where_c.to_string_lossy())?;
        }
        // SAFETY: what is a valid non-null pointer to a VPP vector of u8
        let what_v = unsafe { VecRef::from_raw(self.0.what) };
        let what_str = str::from_utf8(what_v).unwrap_or("<invalid>");
        write!(fmt, "{}", what_str)
    }
}

/// A stack of VPP errors
///
/// This type represents a stack of VPP errors, similar to Rust's [`std::error::Error`]. It
/// typically corresponds to a `clib_error_t *` returned by VPP APIs.
#[derive(Debug)]
pub struct ErrorStack(Vec<Error>);

impl ErrorStack {
    /// Creates an `ErrorStack` directly from a pointer
    ///
    /// # Safety
    /// - The pointer must be valid and point to a VPP vector of type `clib_error_t`.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    pub unsafe fn from_raw(ptr: *mut clib_error_t) -> Self {
        Self(Vec::from_raw(ptr.cast()))
    }

    /// Consumes the error stack and returns a raw pointer
    ///
    /// After calling this method, the caller is responsible for managing the memory of the
    /// vector of errors.
    pub fn into_raw(self) -> *mut clib_error_t {
        let errors = ManuallyDrop::new(self);
        errors.0.as_mut_ptr().cast()
    }

    /// Internal helper to create a new error stack
    ///
    /// # Panics
    ///
    /// Panics if `what` contains nul characters.
    fn new_internal(
        errors: Option<Self>,
        what: String,
        code: Option<crate::bindings::any>,
    ) -> Self {
        let errors_ptr = if let Some(errors) = errors {
            errors.into_raw()
        } else {
            std::ptr::null_mut()
        };
        let what_c = CString::new(what).expect("message should not contain nul characters");
        let flags = if code.is_some() {
            CLIB_ERROR_ERRNO_VALID as uword
        } else {
            0
        };
        let code = code.unwrap_or_default();
        // Note: we cannot populate where_ without using macros in callers due to need for 'static str with a
        // nul-terminator
        // SAFETY: errors_ptr is either null or a valid clib_error_t pointer, what_c is a valid C string
        unsafe {
            Self::from_raw(_clib_error_return(
                errors_ptr,
                code,
                flags,
                std::ptr::null_mut(),
                what_c.as_ptr(),
            ))
        }
    }

    /// Creates a new error stack from a standard error
    ///
    /// The error message is taken from the Display version of the error.
    ///
    /// # Panics
    ///
    /// Panics if the error message contains nul characters.
    pub fn new<E: StdError + Send + Sync + 'static>(e: E) -> Self {
        Self::new_internal(None, e.to_string(), None)
    }

    /// Creates a new error stack from a message
    ///
    /// # Panics
    ///
    /// Panics if the message contains nul characters.
    pub fn msg<M>(message: M) -> Self
    where
        M: Display + Send + Sync + 'static,
    {
        Self::new_internal(None, message.to_string(), None)
    }

    /// Adds context to the error stack
    ///
    /// # Panics
    ///
    /// Panics if the context message contains nul characters.
    pub fn context<C>(self, context: C) -> Self
    where
        C: Display + Send + Sync + 'static,
    {
        Self::new_internal(Some(self), context.to_string(), None)
    }

    /// Returns the errors in the stack.
    pub fn errors(&self) -> &[Error] {
        self.0.as_slice()
    }
}

impl Drop for ErrorStack {
    fn drop(&mut self) {
        for e in self.errors() {
            if e.0.what.is_null() {
                continue;
            }
            // Free the what vec
            // SAFETY: what is a valid non-null pointer to a VPP vector of u8
            let _ = unsafe { Vec::from_raw(e.0.what) };
        }
    }
}

impl Display for ErrorStack {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let errs = self.errors();
        if let Some(err) = errs.last() {
            write!(f, "{}", err)?;
            if errs.len() > 1 {
                write!(f, "\n\nCaused by:")?;
                for err in errs[..errs.len() - 1].iter().rev() {
                    writeln!(f)?;
                    write!(f, "    {}", err)?;
                }
            }
        } else {
            write!(f, "Empty VPP error")?;
        }
        Ok(())
    }
}

impl StdError for ErrorStack {}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::vppinfra::clib_mem_init;

    #[test]
    fn basic() {
        clib_mem_init();

        let io_e = std::io::Error::new(std::io::ErrorKind::AlreadyExists, "Already exists");
        let e = ErrorStack::new(io_e);
        assert_eq!(e.errors().len(), 1);
        assert_eq!(e.to_string(), "Already exists");

        let e = ErrorStack::msg("Unknown interface");
        assert_eq!(e.errors().len(), 1);
        assert_eq!(e.to_string(), "Unknown interface");

        let e = e.context("Failed to enable feature");
        assert_eq!(e.errors().len(), 2);
        assert_eq!(
            e.to_string(),
            "\
Failed to enable feature

Caused by:
    Unknown interface"
        );
    }

    #[test]
    fn ptrs() {
        clib_mem_init();

        let e = ErrorStack::msg("Unknown interface");
        assert_eq!(e.errors().len(), 1);
        assert_eq!(e.to_string(), "Unknown interface");

        let ptr = e.into_raw();
        // SAFETY: ptr was obtained from ErrorStack::into_raw, so must be valid
        let e = unsafe { ErrorStack::from_raw(ptr) };
        assert_eq!(e.errors().len(), 1);
        assert_eq!(e.to_string(), "Unknown interface");
    }
}
