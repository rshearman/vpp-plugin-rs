//! VPP API library
//!
//! Traits, types and helpers for working with API messages and client registrations.

use std::{
    marker::PhantomData,
    mem::{self, MaybeUninit},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
};

use crate::{
    bindings::{
        vl_api_helper_client_index_to_registration, vl_api_helper_send_msg, vl_api_registration_t,
        vl_msg_api_alloc, vl_msg_api_free,
    },
    vlib::BarrierHeldMainRef,
};

/// An owned VPP message buffer containing a `T`.
///
/// The message can be sent to a client using [`Registration::send_message`].
///
/// Important invariant:
///
/// - `T` must have an alignment of 1 (e.g. by `#[repr(packed)]`)
pub struct Message<T> {
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
        let pointer = Message::into_raw(self);
        Message {
            pointer: NonNull::new_unchecked(pointer as *mut T),
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

impl<T> Deref for Message<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        // SAFETY: `self.pointer` was allocated by `vl_msg_api_alloc` and points to a valid,
        // initialised `T` for the lifetime of `&self`.
        unsafe { self.pointer.as_ref() }
    }
}

impl<T> DerefMut for Message<T> {
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

impl<T> Drop for Message<T> {
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

/// Trait used by generated message types that require endian conversions.
///
/// Implementations should swap fields between host and network byte order.
pub trait EndianSwap {
    /// Swap the endianness of the message in-place.
    ///
    /// `to_net == true` indicates conversion from host to network order.
    fn endian_swap(&mut self, to_net: bool);
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
        &mut *(ptr as *mut _)
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
    // FIXME: change &mut to & since we cannot prevent aliases here
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
