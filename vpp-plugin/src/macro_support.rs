//! Types and re-exports to support macros

pub extern crate ctor;
pub extern crate va_list;

/// Converts a string to a NUL-terminated, zero-padded C character array of length `N`, at compile
/// time.
///
/// Used by `vlib_plugin_register!` for registration fields VPP stores inline
/// (`version_required`). In a constant context, fails compilation if the string plus its
/// terminator does not fit or if it contains a NUL byte; at run time those panic.
pub const fn c_char_array<const N: usize>(s: &str) -> [std::os::raw::c_char; N] {
    let bytes = s.as_bytes();
    assert!(
        bytes.len() < N,
        "string does not fit, with its NUL terminator, in the field"
    );
    let mut out = [0 as std::os::raw::c_char; N];
    let mut i = 0;
    while i < bytes.len() {
        assert!(bytes[i] != 0, "string contains a NUL byte");
        out[i] = bytes[i] as std::os::raw::c_char;
        i += 1;
    }
    out
}

#[cfg(test)]
mod tests {
    use super::c_char_array;

    /// As bytes, whatever the signedness of `c_char` on this target.
    fn bytes<const N: usize>(a: [std::os::raw::c_char; N]) -> [u8; N] {
        a.map(|c| c.to_ne_bytes()[0])
    }

    #[test]
    fn pads_and_terminates() {
        const A: [std::os::raw::c_char; 8] = c_char_array::<8>("26.06");
        assert_eq!(bytes(A), [b'2', b'6', b'.', b'0', b'6', 0, 0, 0]);
    }

    #[test]
    fn empty_is_all_zero() {
        assert_eq!(bytes(c_char_array::<4>("")), [0; 4]);
    }

    #[test]
    fn longest_that_fits() {
        assert_eq!(bytes(c_char_array::<4>("abc")), [b'a', b'b', b'c', 0]);
    }

    #[test]
    #[should_panic(expected = "does not fit")]
    fn rejects_no_room_for_terminator() {
        c_char_array::<4>("abcd");
    }

    #[test]
    #[should_panic(expected = "NUL byte")]
    fn rejects_interior_nul() {
        c_char_array::<8>("a\0b");
    }
}
