//! VPP networking errors

use std::{
    ffi::{c_int, CString},
    fmt::Display,
};

use crate::bindings::{vnet_error, vnet_error_t};

/// VPP networking error
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct VnetError(vnet_error_t);

impl VnetError {
    /// Creates an error stack from the VPP networking error and adds context
    ///
    /// # Panics
    ///
    /// Panics if the context message contains nul characters.
    pub fn context<M: Display>(self, message: M) -> crate::vppinfra::error::ErrorStack {
        // SAFETY: pointers passed into vnet_error are valid and are valid nul-terminated C
        // strings. vnet_error aborts on memory allocation error so cannot fail and therefore the
        // returned pointer is always valid.
        unsafe {
            let message_c = CString::new(message.to_string())
                .expect("message should not contain nul terminator");
            let ptr = vnet_error(self.0, c"%s".as_ptr().cast_mut().cast(), message_c.as_ptr());
            crate::vppinfra::error::ErrorStack::from_raw(ptr)
        }
    }
}

impl From<c_int> for VnetError {
    fn from(value: c_int) -> Self {
        Self(value)
    }
}

impl From<VnetError> for i32 {
    fn from(value: VnetError) -> Self {
        value.0
    }
}
