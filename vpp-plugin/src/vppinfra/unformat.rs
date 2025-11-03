//! Generalised string parsing

use std::ffi::c_int;

use crate::bindings::{
    unformat_init_string, unformat_input_t, uword, vlib_helper_unformat_free,
    vlib_helper_unformat_get_input,
};

const UNFORMAT_END_OF_INPUT: uword = !0;

/// Parse a line ending with a newline and return it
///
/// This is similar to the VPP C `unformat_line` function, but without use of varargs and
/// returning a native Rust String.
///
/// This is useful for ensuring debug CLI command functions do not accidentally consume input
/// belonging to other debug CLI commands when invoked as part of a script.
///
/// # Safety
///
/// - The pointer must be valid and point to a properly initialised `unformat_input_t`.
/// - The pointer must stay valid and the contents must not be mutated for the duration of the
///   call.
pub unsafe fn raw_unformat_line_input_to_string(i: *mut unformat_input_t) -> String {
    let mut line = vec![];
    loop {
        let b = vlib_helper_unformat_get_input(i);
        if b == b'\n'.into() || b == UNFORMAT_END_OF_INPUT {
            break;
        }
        line.push(b as u8);
    }
    String::from_utf8_lossy(&line).to_string()
}

/// Input to an unformat operation
///
/// This corresponds to the VPP C type of `unformat_input_t`.
pub struct UnformatInput(unformat_input_t);

impl UnformatInput {
    /// Returns a raw pointer to the underlying `unformat_input_t`
    pub fn as_ptr(&mut self) -> *mut unformat_input_t {
        &mut self.0
    }
}

impl From<&str> for UnformatInput {
    fn from(value: &str) -> Self {
        let mut me = Self(Default::default());
        // SAFETY: the string pointer is valid and the length passed in is consistent.
        // unformat_init_string copies the contents of the string into a vec no restrictions are
        // needed on the lifetime of value.
        unsafe {
            unformat_init_string(&mut me.0, value.as_ptr().cast(), value.len() as c_int);
        }
        me
    }
}

impl From<String> for UnformatInput {
    fn from(value: String) -> Self {
        value.as_str().into()
    }
}

impl Drop for UnformatInput {
    fn drop(&mut self) {
        // SAFETY: this is a valid unformat_input_t which hasn't been previously freed
        unsafe {
            vlib_helper_unformat_free(self.as_ptr());
        }
    }
}
