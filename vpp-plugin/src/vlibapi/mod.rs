//! VPP API library
//!
//! Traits, types and helpers for working with API messages and client registrations.

use std::{
    borrow::Cow,
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
    slice,
    str::Utf8Error,
};

use crate::{
    bindings::{
        vl_api_helper_client_index_to_registration, vl_api_helper_send_msg, vl_api_registration_t,
        vl_msg_api_alloc, vl_msg_api_free,
    },
    vlib::BarrierHeldMainRef,
};

pub mod num_unaligned;

/// An owned VPP message buffer containing a `T`.
///
/// The message can be sent to a client using [`Registration::send_message`].
///
/// Important invariant:
///
/// - `T` must have an alignment of 1 (e.g. by `#[repr(packed)]`)
pub struct Message<T: ?Sized> {
    pointer: NonNull<T>,
}

impl<T> Message<T> {
    /// Allocate a VPP message and initialise it by copying `value` into the
    /// newly-allocated buffer.
    ///
    /// # Panics
    ///
    /// Panics if `align_of::<T>() != 1` because the VPP API message allocator does not provide
    /// alignment guarantees; generated message structs are expected to be packed.
    pub fn new(value: T) -> Self {
        if std::mem::align_of::<T>() != 1 {
            // It's unclear what alignment guarantees vpp gives. Possibly the memory is aligned
            // to align_of(msghdr_t), but play it safe and required packed types -
            // vpp-plugin-api-gen generated types always have this anyway.
            panic!("Messages must only contain #[repr(packed)] types");
        }

        // SAFETY: `vl_msg_api_alloc` returns a pointer to at least `size_of::<T>()` bytes (or
        // null on allocation failure). We have asserted `align_of::<T>() == 1` so the cast to
        // `*mut T` is valid. It is safe to use `NonNull::new_unchecked` because the VPP
        // allocation cannot fail and instead aborts on allocation failures.
        unsafe {
            let mut me = Self {
                pointer: NonNull::new_unchecked(
                    vl_msg_api_alloc(std::mem::size_of::<T>() as i32) as *mut T
                ),
            };
            ptr::copy_nonoverlapping(&value, me.pointer.as_mut(), 1);
            me
        }
    }

    /// Allocate an uninitialised VPP message buffer for `T`.
    ///
    /// This returns a `Message<MaybeUninit<T>>`. Use [`Self::write`] or [`Self::assume_init`]
    /// after manually initialising the contents.
    ///
    ///
    /// # Panics
    ///
    /// Panics if `align_of::<T>() != 1` for the same reason as `new`.
    pub fn new_uninit() -> Message<MaybeUninit<T>> {
        if std::mem::align_of::<T>() != 1 {
            // It's unclear what alignment guarantees vpp gives. Possibly the memory is aligned
            // to align_of(msghdr_t), but play it safe and required packed types -
            // vpp-plugin-api-gen generated types always have this anyway.
            panic!("Messages must only contain #[repr(packed)] types");
        }

        // SAFETY: `vl_msg_api_alloc` returns a pointer to at least `size_of::<MaybeUninit<T>>()`
        // bytes. Casting that pointer to `*mut MaybeUninit<T>` is valid as the buffer is
        // uninitialised but suitably sized. It is safe to use `NonNull::new_unchecked` because
        // the VPP allocation cannot fail and instead aborts on allocation failures.
        unsafe {
            Message {
                pointer: NonNull::new_unchecked(vl_msg_api_alloc(
                    std::mem::size_of::<MaybeUninit<T>>() as i32,
                ) as *mut MaybeUninit<T>),
            }
        }
    }
}

impl Message<u8> {
    /// Allocate a VPP message buffer `nbytes` of `u8`s initialised to 0.
    pub fn new_bytes(nbytes: u32) -> Message<u8> {
        // SAFETY: `vl_msg_api_alloc` returns a pointer to at least `size_of::<MaybeUninit<T>>()`
        // bytes. Casting that pointer to `*mut MaybeUninit<T>` is valid as the buffer is
        // uninitialised but suitably sized. It is safe to use `NonNull::new_unchecked` because
        // the VPP allocation cannot fail and instead aborts on allocation failures.
        unsafe {
            let mut me = Message {
                pointer: NonNull::new_unchecked(vl_msg_api_alloc(nbytes as i32) as *mut u8),
            };
            ptr::write_bytes(me.pointer.as_mut(), 0, nbytes as usize);
            me
        }
    }
}

impl<T: ?Sized> Message<T> {
    /// Consume the `Message` and return the raw pointer to the underlying buffer
    ///
    /// The returned pointer becomes the caller's responsibility. The `Message` destructor will
    /// not run for `m` and the underlying buffer will not be freed by Rust; callers must ensure
    /// the buffer is eventually freed (for example by passing it to VPP or calling
    /// `vl_msg_api_free`).
    ///
    /// Not a method on `Message` to avoid clashing with application methods of the same name on
    /// the underlying type.
    pub fn into_raw(m: Self) -> *mut T {
        let m = mem::ManuallyDrop::new(m);
        m.pointer.as_ptr()
    }
}

impl<T> Message<MaybeUninit<T>> {
    /// Convert a `Message<MaybeUninit<T>>` into a `Message<T>` without performing any
    /// initialisation checks
    ///
    /// # Safety
    ///
    /// The caller must ensure that the underlying buffer is fully initialised for `T`. If the
    /// memory is not properly initialised, using the resulting `Message<T>` is undefined
    /// behaviour.
    pub unsafe fn assume_init(self) -> Message<T> {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe {
            let pointer = Message::into_raw(self);
            Message {
                pointer: NonNull::new_unchecked(pointer as *mut T),
            }
        }
    }

    /// Initialise the previously-uninitialised buffer with `value` and return the initialised
    /// `Message<T>`
    pub fn write(mut self, value: T) -> Message<T> {
        // SAFETY: We have exclusive ownership of the allocated buffer for
        // `self`. Writing `value` into the `MaybeUninit<T>` buffer
        // initialises it, after which `assume_init` converts the message to
        // `Message<T>`.
        unsafe {
            (*self).write(value);
            self.assume_init()
        }
    }
}

impl<T: ?Sized> Deref for Message<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: `self.pointer` was allocated by `vl_msg_api_alloc` and points to a valid,
        // initialised `T` for the lifetime of `&self`.
        unsafe { self.pointer.as_ref() }
    }
}

impl<T: ?Sized> DerefMut for Message<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        // SAFETY: `self.pointer` was allocated by `vl_msg_api_alloc` and we hold exclusive access
        // via `&mut self`, so returning a mutable reference to the inner `T` is valid.
        unsafe { self.pointer.as_mut() }
    }
}

impl<T: Default> Default for Message<T> {
    fn default() -> Self {
        Self::new_uninit().write(Default::default())
    }
}

impl<T: ?Sized> Drop for Message<T> {
    fn drop(&mut self) {
        // SAFETY: We own the underlying buffer and the memory is considered initialised for `T`
        // at time of drop. It's therefore safe to drop the contained `T` and free the buffer
        // with the VPP message API free function.
        unsafe {
            ptr::drop_in_place(self.pointer.as_ptr());
            vl_msg_api_free(self.pointer.as_ptr().cast());
        }
    }
}

impl<T> From<T> for Message<T> {
    fn from(value: T) -> Self {
        Self::new(value)
    }
}

impl<T: ?Sized + PartialOrd> PartialOrd for Message<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        (**self).partial_cmp(&**other)
    }
}

impl<T: ?Sized + Ord> Ord for Message<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (**self).cmp(&**other)
    }
}

impl<T: ?Sized + PartialEq> PartialEq for Message<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: ?Sized + Eq> Eq for Message<T> {}

impl<T: ?Sized + Hash> Hash for Message<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (**self).hash(state);
    }
}

impl<T: fmt::Display + ?Sized> fmt::Display for Message<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&**self, f)
    }
}

impl<T: fmt::Debug + ?Sized> fmt::Debug for Message<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&**self, f)
    }
}

impl<T: ?Sized> fmt::Pointer for Message<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let ptr: *const T = &**self;
        fmt::Pointer::fmt(&ptr, f)
    }
}

/// Trait used by generated message types that require endian conversions.
///
/// Implementations should swap fields between host and network byte order.
pub trait EndianSwap {
    /// Swap the endianness of the message in-place.
    ///
    /// `to_net == true` indicates conversion from host to network order.
    ///
    /// # Safety
    ///
    /// The caller must ensure that if `self` contains a variable length array that elements
    /// indexed from 0 up to the contents of the length field are initialised and contained within
    /// the memory allocated for the object.
    unsafe fn endian_swap(&mut self, to_net: bool);
}

/// Registration state for the VPP side of an API client
///
/// A `&mut Registration` corresponds to a C `vl_api_registration *`.
///
/// Use [`RegistrationScope::from_client_index`] to obtain a mutable reference.
#[repr(transparent)]
pub struct Registration(foreign_types::Opaque);

impl Registration {
    /// Construct a `&mut Registration` from a raw `vl_api_registration_t` pointer.
    ///
    /// # Safety
    ///
    /// - `ptr` must be a valid, non-null pointer to a `vl_api_registration_t`.
    /// - The caller must ensure exclusive mutable access for the returned lifetime `'a` (no other
    ///   references or concurrent uses may alias the same underlying registration for the
    ///   duration of the returned borrow).
    /// - The pointer must remain valid for the returned lifetime and must not be freed or
    ///   invalidated while the borrow is active.
    pub unsafe fn from_ptr_mut<'a>(ptr: *mut vl_api_registration_t) -> &'a mut Self {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe { &mut *(ptr as *mut _) }
    }

    /// Return the raw `vl_api_registration_t` pointer for this `Registration`.
    pub fn as_ptr(&self) -> *mut vl_api_registration_t {
        self as *const _ as *mut _
    }

    /// Send a message to the registration.
    ///
    /// This consumes `message` and transfers ownership of the underlying buffer to VPP.
    pub fn send_message<T>(&mut self, message: Message<T>) {
        // SAFETY: `self.as_ptr()` returns a raw `vl_api_registration_t` pointer that is valid
        // for the duration of this call. `Message::into_raw` transfers ownership of the message
        // buffer and yields a pointer that is safe to pass to the C API; the C API takes
        // ownership of the buffer. `vl_api_helper_send_msg` is called with valid pointers.
        unsafe {
            vl_api_helper_send_msg(self.as_ptr(), Message::into_raw(message).cast());
        }
    }
}

/// Scope helper used to obtain short-lived `&mut Registration` borrows.
///
/// This enforces that `Registration` references obtained cannot be retained beyond the
/// `registration_scope` function call
pub struct RegistrationScope<'scope>(PhantomData<&'scope ()>);

impl<'scope> RegistrationScope<'scope> {
    /// Look up a `Registration` by VPP client index.
    ///
    /// Returns `Some(&mut Registration)` when the client index corresponds to a current
    /// registration, or `None` if no registration exists for that index.
    pub fn from_client_index(
        &self,
        _vm: &BarrierHeldMainRef,
        client_index: u32,
    ) -> Option<&'scope mut Registration> {
        // SAFETY: `vl_api_helper_client_index_to_registration` returns either a null pointer or
        // a valid pointer to a `vl_api_registration_t` that lives as long as the corresponding
        // client registration in VPP. The lifetime of the returned reference ensures the caller
        // cannot retain that reference beyond the intended scope.
        unsafe {
            let ptr = vl_api_helper_client_index_to_registration(client_index.to_be());
            if ptr.is_null() {
                None
            } else {
                Some(Registration::from_ptr_mut(ptr))
            }
        }
    }
}

/// Execute a closure with a temporary `RegistrationScope`.
///
/// Used to ensure any `&mut Registration` borrows that are obtained are tied to the lifetime of
/// the closure and cannot accidentally escape.
pub fn registration_scope<F, T>(f: F) -> T
where
    F: for<'scope> FnOnce(&'scope RegistrationScope<'scope>) -> T,
{
    let scope = RegistrationScope(PhantomData);
    f(&scope)
}

/// A stream for sending messages to a registration
pub struct Stream<'scope, T> {
    registration: &'scope mut Registration,
    _phantom: PhantomData<T>,
}

impl<'scope, T> Stream<'scope, T> {
    /// Creates a new stream from a mutable reference to a registration.
    pub fn new(registration: &'scope mut Registration) -> Self {
        Self {
            registration,
            _phantom: PhantomData,
        }
    }

    /// Sends a network-endian (big-endian) message to the registration
    ///
    /// Since the message is in network order, then it is sent without performing an endian swap.
    pub fn send_message_ne(&mut self, message: Message<T>) {
        self.registration.send_message(message);
    }

    /// Consumes the stream and returns the underlying registration reference.
    pub fn into_inner(self) -> &'scope mut Registration {
        self.registration
    }
}

impl<'scope, T: EndianSwap> Stream<'scope, T> {
    /// Sends a message to the registration after performing endian swap to network order.
    ///
    /// # Safety
    ///
    /// The caller must ensure that if `messages` contains a variable length array that elements
    /// indexed from 0 up to the contents of the length field are initialised and contained within
    /// the memory allocated for the object.
    pub unsafe fn send_message(&mut self, mut message: Message<T>) {
        // SAFETY: The safety requirements are documented in the function's safety comment.
        unsafe {
            message.endian_swap(true);
            self.send_message_ne(message);
        }
    }
}

#[repr(C, packed)]
#[derive(Default)]
#[allow(missing_copy_implementations)] // see below
/// A string type used in VPP API messages.
///
/// This represents a variable-length string with a length prefix,
/// commonly used in VPP API message structures.
///
/// The contents of the string are stored immediately after the length, in the memory of the
/// containing message, so an `ApiString` is only meaningful in place in that memory. For this
/// reason it is neither `Copy` nor `Clone`, and neither are types that contain it, since a copy
/// would contain the length but not the contents.
pub struct ApiString {
    length: u32,
    buf: [u8; 0],
}

impl ApiString {
    /// Returns the length of the string in bytes.
    pub const fn len(&self) -> u32 {
        self.length
    }

    /// Returns `true` if the string has a length of zero.
    pub const fn is_empty(&self) -> bool {
        self.length == 0
    }

    /// Returns a byte slice of the string's contents.
    pub fn as_bytes(&self) -> &[u8] {
        // SAFETY: The buffer memory is valid and initialised for at least self.length bytes.
        unsafe {
            slice::from_raw_parts(
                std::ptr::addr_of!(self.buf) as *const u8,
                self.length as usize,
            )
        }
    }

    /// Returns a mutable byte slice of the string's contents.
    fn as_bytes_mut(&mut self) -> &mut [u8] {
        // SAFETY: The buffer memory is valid and initialised for at least self.length bytes.
        unsafe {
            slice::from_raw_parts_mut(
                std::ptr::addr_of_mut!(self.buf) as *mut u8,
                self.length as usize,
            )
        }
    }

    /// Converts the string to a `&str` slice.
    ///
    /// If the contents of the `ApiString` are valid UTF-8 data, this
    /// function will return the corresponding `&[str]` slice. Otherwise,
    /// it will return an error with details of where UTF-8 validation failed.
    pub fn to_str(&self) -> Result<&str, Utf8Error> {
        str::from_utf8(self.as_bytes())
    }

    /// Converts the string to a `Cow<str>`, replacing invalid UTF-8 sequences with �.
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.as_bytes())
    }

    /// Sets the length of the string in bytes.
    ///
    /// # Safety
    ///
    /// The caller must ensure that the underlying buffer has at least `length` bytes of valid
    /// memory and is initialised.
    pub unsafe fn set_len(&mut self, length: u32) {
        self.length = length;
    }

    /// Copies the contents of the given string into this `ApiString`.
    ///
    /// # Panics
    ///
    /// Panics if the length of the `ApiString` is different to the length of the string in bytes.
    pub fn copy_from_str(&mut self, s: &str) {
        self.as_bytes_mut().copy_from_slice(s.as_bytes());
    }
}

impl std::fmt::Debug for ApiString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.to_string_lossy(), f)
    }
}

impl EndianSwap for ApiString {
    unsafe fn endian_swap(&mut self, _to_net: bool) {
        self.length = self.length.to_be();
        // No endian swap necessary for self.buf
    }
}

#[repr(C)]
#[derive(Copy, Clone, PartialEq, Eq)]
/// A string type used in VPP API messages.
///
/// This represents a fixed-length, nul-terminated string,
/// commonly used in VPP API message structures.
pub struct ApiFixedString<const N: usize> {
    buf: [u8; N],
}

impl<const N: usize> ApiFixedString<N> {
    /// Returns the length of the string in bytes.
    pub const fn len(&self) -> usize {
        let mut len = 0;
        while self.buf[len] != 0 {
            len += 1;
        }
        len
    }

    /// Returns `true` if the string has a length of zero.
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns a byte slice of the string's contents not including the nul-terminator.
    pub fn as_bytes(&self) -> &[u8] {
        &self.buf[..self.len()]
    }

    /// Converts the string to a `&str` slice.
    ///
    /// If the contents of the `ApiFixedString` are valid UTF-8 data, this
    /// function will return the corresponding `&[str]` slice. Otherwise,
    /// it will return an error with details of where UTF-8 validation failed.
    pub fn to_str(&self) -> Result<&str, Utf8Error> {
        str::from_utf8(self.as_bytes())
    }

    /// Converts the string to a `Cow<str>`, replacing invalid UTF-8 sequences with �.
    pub fn to_string_lossy(&self) -> Cow<'_, str> {
        String::from_utf8_lossy(self.as_bytes())
    }

    /// Copies the contents of the given string into this `ApiFixedString`.
    ///
    /// # Panics
    ///
    /// Panics if the string exceeds the capacity of the fixed buffer.
    pub fn copy_from_str(&mut self, s: &str) {
        let bytes = s.as_bytes();
        self.buf[0..bytes.len()].copy_from_slice(bytes);
        // Set any remaining elements to 0 since they are serialised to the wire and so we don't
        // want any stale data, plus the first 0 acts as a nul-terminator.
        if bytes.len() + 1 < self.buf.len() {
            self.buf[bytes.len()..].fill(0);
        }
    }
}

impl<const N: usize> std::fmt::Debug for ApiFixedString<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.to_string_lossy(), f)
    }
}

impl<const N: usize> EndianSwap for ApiFixedString<N> {
    unsafe fn endian_swap(&mut self, _to_net: bool) {
        // No endian swap necessary for self.buf
    }
}

impl<const N: usize> Default for ApiFixedString<N> {
    fn default() -> Self {
        Self { buf: [0; N] }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::borrow::Cow;

    #[test]
    fn test_fixed_string_default() {
        let s: ApiFixedString<10> = Default::default();
        assert_eq!(s.len(), 0);
        assert!(s.is_empty());
        assert_eq!(s.as_bytes(), &[]);
        assert_eq!(s.to_string_lossy(), Cow::Borrowed(""));
    }

    #[test]
    fn test_fixed_string_copy_from_str() {
        let mut s: ApiFixedString<10> = Default::default();
        s.copy_from_str("hello");
        assert_eq!(s.len(), 5);
        assert!(!s.is_empty());
        assert_eq!(s.as_bytes(), b"hello");
        assert_eq!(s.to_string_lossy(), Cow::Borrowed("hello"));
    }

    #[test]
    fn test_fixed_string_copy_from_str_with_padding() {
        let mut s: ApiFixedString<10> = Default::default();
        s.copy_from_str("hi");
        assert_eq!(s.len(), 2);
        assert_eq!(s.as_bytes(), b"hi");
        // Check that the rest is zeroed
        assert_eq!(s.buf[2..], [0; 8]);
    }

    #[test]
    fn test_fixed_string_copy_from_str_empty() {
        let mut s: ApiFixedString<10> = Default::default();
        s.copy_from_str("");
        assert_eq!(s.len(), 0);
        assert!(s.is_empty());
    }

    #[test]
    fn test_fixed_string_copy_from_str_max_length() {
        let mut s: ApiFixedString<5> = Default::default();
        s.copy_from_str("abcd"); // 4 chars, should fit with nul
        assert_eq!(s.len(), 4);
        assert_eq!(s.as_bytes(), b"abcd");
    }

    #[test]
    #[should_panic]
    fn test_fixed_string_copy_from_str_too_long() {
        let mut s: ApiFixedString<5> = Default::default();
        s.copy_from_str("abcdef"); // 6 chars, too long
    }

    #[test]
    fn test_fixed_string_to_str_valid_utf8() {
        let mut s: ApiFixedString<10> = Default::default();
        s.copy_from_str("hello");
        assert_eq!(s.to_str().unwrap(), "hello");
    }

    #[test]
    fn test_fixed_string_to_str_invalid_utf8() {
        let mut s: ApiFixedString<10> = Default::default();
        // Manually set invalid UTF-8
        s.buf[0] = 0xff;
        s.buf[1] = 0xfe;
        s.buf[2] = 0;
        assert!(s.to_str().is_err());
    }

    #[test]
    fn test_fixed_string_to_string_lossy_invalid_utf8() {
        let mut s: ApiFixedString<10> = Default::default();
        s.copy_from_str("hello");
        assert_eq!(s.to_string_lossy(), "hello");

        // Invalid UTF-8
        s.buf[0] = 0xff;
        s.buf[1] = 0;
        assert_eq!(s.to_string_lossy(), "�");
    }

    #[test]
    fn test_fixed_string_debug() {
        let mut s: ApiFixedString<10> = Default::default();
        s.copy_from_str("test");
        assert_eq!(format!("{:?}", s), "\"test\"");
    }

    #[test]
    fn test_string_in_place() {
        // Length prefix in host order followed by the contents, as in a message buffer
        let mut buf = [0u8; 4 + 5];
        buf[..4].copy_from_slice(&5u32.to_ne_bytes());
        buf[4..].copy_from_slice(b"hello");
        // SAFETY: ApiString has an alignment of 1 and buf contains the length followed by that
        // many bytes of contents
        let s = unsafe { &*(buf.as_ptr() as *const ApiString) };
        assert_eq!(s.len(), 5);
        assert!(!s.is_empty());
        assert_eq!(s.as_bytes(), b"hello");
        assert_eq!(s.to_str().unwrap(), "hello");
        assert_eq!(format!("{:?}", s), "\"hello\"");
    }

    #[test]
    fn test_fixed_string_partialeq() {
        // Fill part of the rest of the string to ensure it has no effect when a shorter string is
        // copied over
        let mut s1: ApiFixedString<10> = Default::default();
        s1.copy_from_str("test");
        assert_eq!(s1.to_string_lossy(), "test");

        s1.copy_from_str("te");

        let mut s2: ApiFixedString<10> = Default::default();
        s2.copy_from_str("te");

        assert_eq!(s1, s2);
    }
}
