//! Dynamically resizable arrays
//!
//! This module provides abstractions around VPP's vector type, which is a
//! dynamically resizable array similar to Rust's `Vec<T>`.
//!
//! # Memory layout
//!
//! ```text
//! +-------------------+----------------+----------------+----------------+
//! | vec_header_t hdr  | T elem[0]      | T elem[1]      | T elem[2]      | ...
//! +-------------------+----------------+----------------+----------------+
//!                     ^
//!                     +---------------- User pointer
//! ```
//! # Relationship between `Vec<T>` and `VecRef<T>`
//!
//! The `Vec<T>` type is an owning vector type that manages the memory
//! allocation and deallocation for the vector. It provides methods for
//! modifying the vector, such as `push`, `pop`, and `insert_mut`.
//! The `VecRef<T>` type is a non-owning reference type that provides
//! read-only access to the elements of the vector. It is used when
//! you want to pass around a reference to a vector without taking ownership of it.
//!
//! Note that there may be some times when you don't own the vector, but you need
//! to perform operations which may resize it, such as to push an element. Be careful about
//! updating the original pointer because if the vector is resized the pointer may change and
//! the original pointer may no longer be valid. In these cases, you can temporarily take
//! ownership of the vector by doing:
//!
//! ```rust
//! use vpp_plugin::vppinfra::vec::Vec;
//! # use vpp_plugin::vec;
//!
//! # vpp_plugin::vppinfra::clib_mem_init();
//! # let mut ptr = vec![1, 2, 3].into_raw();
//! let mut v = unsafe { Vec::from_raw(ptr) };
//! v.push(1);
//! ptr = v.into_raw();
//! # unsafe { Vec::from_raw(ptr); } // prevent leak
//! ```

use std::{
    borrow::{Borrow, BorrowMut},
    cmp::Ordering,
    fmt,
    mem::{ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut},
    ptr::{self, NonNull},
    slice,
};

use crate::bindings::{
    _vec_alloc_internal, _vec_resize_internal, vec_attr_t, vec_free_not_inline, vec_header_t,
    VEC_MIN_ALIGN,
};

/// Reference to a dynamically resizable array
///
/// A `&VecRef<T>` is equivalent to a `T *` pointer in VPP (`*mut T` in Rust parlance).
///
/// Due to the memory layout of VPP vectors, this type provides methods to access the vector and
/// modify it as long as it never needs resizing. If resizing is needed, you must use the
/// [`Vec<T>`] type.
pub struct VecRef<T>(foreign_types::Opaque, std::marker::PhantomData<T>);

impl<T> VecRef<T> {
    /// Creates a `&VecRef<T>` directly from a pointer
    ///
    /// # Safety
    /// - The pointer must be valid and point to a VPP vector of type `T`.
    /// - `T` needs to have the alignment that is less than or equal to the alignment of elements
    ///   used when allocating the vector.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned reference.
    ///
    /// See also [`Self::from_raw_mut`].
    #[inline(always)]
    pub unsafe fn from_raw<'a>(ptr: *const T) -> &'a Self {
        &*(ptr as *mut _)
    }

    /// Creates a `&mut VecRef<T>` directly from a pointer
    ///
    /// # Safety
    /// - The pointer must be valid and point to a VPP vector of type `T`.
    /// - `T` needs to have the alignment that is less than or equal to the alignment of elements
    ///   used when allocating the vector.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned reference.
    ///
    /// See also [`Self::from_raw`].
    #[inline(always)]
    pub unsafe fn from_raw_mut<'a>(ptr: *mut T) -> &'a mut Self {
        &mut *(ptr as *mut _)
    }

    /// Returns a raw pointer to the first element of the vector
    ///
    /// The caller must ensure that the vector outlives the returned pointer. Modifying the vector
    /// may invalidate the pointer.
    ///
    /// The caller must ensure that the pointer is not used to mutate the vector (except inside an
    /// `UnsafeCell`).
    ///
    /// If you need a mutable pointer, use [`Self::as_mut_ptr`] instead.
    #[inline(always)]
    pub const fn as_ptr(&self) -> *const T {
        self as *const _ as *const _
    }

    /// Returns a raw pointer to the first element of the vector
    ///
    /// The caller must ensure that the vector outlives the returned pointer. Modifying the vector
    /// may invalidate the pointer.
    ///
    /// The caller must ensure that the pointer is not used to mutate the vector (except inside an
    /// `UnsafeCell`).
    ///
    /// If you need a mutable pointer, use [`Self::as_mut_ptr`] instead.
    #[inline(always)]
    pub const fn as_mut_ptr(&self) -> *mut T {
        self as *const _ as *mut _
    }

    fn as_vec_header_ptr(&self) -> *mut vec_header_t {
        // SAFETY: creation preconditions mean this is a valid object and so the vec_header_t is
        // located just before the user pointer
        unsafe {
            let ptr: *mut vec_header_t = self.as_mut_ptr().cast();
            ptr.sub(1)
        }
    }

    /// Returns the length of the vector
    #[inline]
    pub fn len(&self) -> usize {
        // SAFETY: creation preconditions mean this is a valid object and it's safe to read the
        // len field
        unsafe { (*self.as_vec_header_ptr()).len as usize }
    }

    /// Returns true if the vector is empty
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the capacity of the vector
    ///
    /// This is the total number of elements that can be stored in the vector without resizing.
    pub fn capacity(&self) -> usize {
        // SAFETY: creation preconditions mean this is a valid object and it's safe to access the
        // header
        unsafe {
            let hdr = self.as_vec_header_ptr();
            ((*hdr).len + (*hdr).grow_elts as u32) as usize
        }
    }

    /// Removes and returns the element at the specified index
    ///
    /// # Panics
    ///
    /// Panics if `index` is out of bounds.
    pub fn remove(&mut self, index: usize) -> T {
        let hdr = self.as_vec_header_ptr();
        // SAFETY: creation preconditions mean this is a valid object, we ensure index is in
        // bounds and on exit the length is decremented so the dropped element cannot be accessed
        // again
        unsafe {
            let len = (*hdr).len as usize;

            assert!(
                index < len,
                "removal index (is {index}) should be <= len (is {len})"
            );

            // the place we are taking from.
            let ptr = self.as_mut_ptr().add(index);
            // copy it out, unsafely having a copy of the value on
            // the stack and in the vector at the same time.
            let ret = ptr::read(ptr);

            // Shift everything down to fill in that spot.
            ptr::copy(ptr.add(1), ptr, len - index - 1);

            (*hdr).len = len as u32 - 1;
            (*hdr).grow_elts += 1;
            ret
        }
    }

    /// Removes and returns the last element of the vector, if any
    pub fn pop(&mut self) -> Option<T> {
        let hdr = self.as_vec_header_ptr();
        // SAFETY: creation preconditions mean this is a valid object, and on exit the length is
        // decremented so the dropped element cannot be accessed again
        unsafe {
            let len = (*hdr).len as usize;
            if len == 0 {
                None
            } else {
                (*hdr).len = len as u32 - 1;
                (*hdr).grow_elts += 1;
                Some(ptr::read(self.as_ptr().add(len - 1)))
            }
        }
    }

    /// Clears the vector, removing all elements
    ///
    /// Note this has no effect on the capacity of the vector.
    pub fn clear(&mut self) {
        let elems: *mut [T] = self.as_mut_slice();
        // SAFETY: creation preconditions mean this is a valid object, and on exit the length is
        // set to zero so the dropped elements cannot be accessed again
        unsafe {
            let hdr = self.as_vec_header_ptr();
            let len = (*hdr).len;
            (*hdr).len = 0;
            (*hdr).grow_elts = len.try_into().unwrap_or(u8::MAX);
            ptr::drop_in_place(elems);
        }
    }

    /// Returns the contents of the vector as a slice
    ///
    /// Equivalent to `&vec[..]`.
    ///
    /// See also [`Self::as_mut_slice`].
    pub fn as_slice(&self) -> &[T] {
        // SAFETY: creation preconditions mean this is a valid object and contiguous and
        // initialised up to len
        unsafe { slice::from_raw_parts(self.as_ptr(), self.len()) }
    }

    /// Returns the contents of the vector as a mutable slice
    ///
    /// Equivalent to `&mut vec[..]`.
    ///
    /// See also [`Self::as_slice`].
    pub fn as_mut_slice(&mut self) -> &mut [T] {
        // SAFETY: creation preconditions mean this is a valid object and contiguous and
        // initialised up to len
        unsafe { slice::from_raw_parts_mut(self.as_mut_ptr(), self.len()) }
    }

    /// Returns the allocated spare capacity of the vector as a slice of `MaybeUninit<T>`
    ///
    /// This can be used to initialize multiple elements at once before updating the length
    /// using [`Self::set_len`].
    pub fn spare_capacity_mut(&mut self) -> &mut [MaybeUninit<T>] {
        // SAFETY: creation preconditions mean this is a valid object and contiguous and
        // memory is allocated up to capacity, and the use of MaybeUninit<T> ensures that the
        // uninitialised elements cannot be read by safe code.
        unsafe {
            slice::from_raw_parts_mut(
                self.as_ptr().add(self.len()) as *mut MaybeUninit<T>,
                self.capacity() - self.len(),
            )
        }
    }

    /// Sets the length of the vector
    ///
    /// # Safety
    /// - `new_len` must be less than or equal to the capacity of the vector.
    /// - The elements up to `new_len` must be initialized.
    pub unsafe fn set_len(&mut self, new_len: usize) {
        debug_assert!(new_len <= self.capacity());
        let hdr = self.as_vec_header_ptr();
        let new_grow_elts = self.capacity() - new_len;
        (*hdr).len = new_len as u32;
        (*hdr).grow_elts = new_grow_elts.try_into().unwrap_or(u8::MAX);
    }
}

impl<T> Deref for VecRef<T> {
    type Target = [T];

    fn deref(&self) -> &[T] {
        self.as_slice()
    }
}

impl<T> DerefMut for VecRef<T> {
    fn deref_mut(&mut self) -> &mut [T] {
        self.as_mut_slice()
    }
}

impl<T: fmt::Debug> fmt::Debug for VecRef<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_slice(), f)
    }
}

impl<T: Clone> ToOwned for VecRef<T> {
    type Owned = Vec<T>;

    fn to_owned(&self) -> Vec<T> {
        self.to_vpp_vec()
    }
}

impl<T> AsRef<VecRef<T>> for VecRef<T> {
    fn as_ref(&self) -> &VecRef<T> {
        self
    }
}

impl<T> AsMut<VecRef<T>> for VecRef<T> {
    fn as_mut(&mut self) -> &mut VecRef<T> {
        self
    }
}

impl<T> AsRef<[T]> for VecRef<T> {
    fn as_ref(&self) -> &[T] {
        self
    }
}

impl<T> AsMut<[T]> for VecRef<T> {
    fn as_mut(&mut self) -> &mut [T] {
        self
    }
}

impl<T> PartialOrd<VecRef<T>> for VecRef<T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &VecRef<T>) -> Option<Ordering> {
        PartialOrd::partial_cmp(&**self, &**other)
    }
}

impl<T: Eq> Eq for VecRef<T> {}

impl<T: Ord> Ord for VecRef<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&**self, &**other)
    }
}

// SAFETY: it's fine to deallocate the memory from another thread, and fine to access Vec<T> from
// another thread as long as T is Send
unsafe impl<T> Send for VecRef<T> where T: Send {}

/// Owned dynamically resizable array
///
/// A `Vec<T>` is equivalent to a `T *` pointer in VPP (`*mut T` in Rust parlance).
pub struct Vec<T>(NonNull<T>);

impl<T> Vec<T> {
    /// Creates a `Vec<T>` directly from a pointer
    ///
    /// # Safety
    /// - The pointer must be valid and point to a VPP vector of type `T`.
    /// - `T` needs to have the alignment that is less than or equal to the alignment of elements
    ///   used when allocating the vector.
    /// - The pointer must stay valid and the contents must not be mutated for the duration of the
    ///   lifetime of the returned object.
    pub unsafe fn from_raw(ptr: *mut T) -> Vec<T> {
        Vec(NonNull::new_unchecked(ptr))
    }

    /// Creates a new vector with the at least the specified capacity
    ///
    /// The created vector will have a length of 0.
    ///
    /// Note that the capacity of the created vector may be larger than the requested capacity.
    pub fn with_capacity(capacity: usize) -> Self {
        let align = std::mem::align_of::<T>() as u16;
        let attr = vec_attr_t {
            align: align.max(VEC_MIN_ALIGN as u16),
            hdr_sz: 0, // TODO
            heap: std::ptr::null_mut(),
            elt_sz: std::mem::size_of::<T>() as u32,
        };
        // SAFETY: we ensure that the attributes are valid for T, and _vec_alloc_internal returns a valid
        // pointer or aborts, and the pointer has memory laid out as we expect, i.e. the
        // vec_header_t before it.
        unsafe {
            let mut v = Self(NonNull::new_unchecked(
                _vec_alloc_internal(capacity as u64, &attr).cast(),
            ));
            // vpp sets len to the alloc'd len, but the semantics of this method is such that len should only be those that are in use
            v.set_len(0);
            v
        }
    }

    /// Creates a new, empty vector
    pub fn new() -> Self {
        Self::with_capacity(0)
    }

    /// Reserves capacity for at least `additional` more elements to be inserted
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `u32`.
    pub fn reserve(&mut self, additional: usize) {
        let align = std::mem::align_of::<T>() as u16;
        let len = self.len();
        let new_len = len.checked_add(additional).unwrap_or_else(|| {
            panic!("Overflow on adding {additional} elements to vec of len {len}")
        });
        // u32 is the len field in the header, despite _vec_size_internal taking a u64
        let new_len = u32::try_from(new_len).unwrap_or_else(|_| {
            panic!("Overflow on adding {additional} elements to vec of len {len}")
        });
        let attr = vec_attr_t {
            align: align.max(VEC_MIN_ALIGN as u16),
            hdr_sz: 0, // TODO
            heap: std::ptr::null_mut(),
            elt_sz: std::mem::size_of::<T>() as u32,
        };
        // SAFETY: we ensure that the attributes are valid for T, pass a valid pointer (with a
        // vec_header_t before it) to _vec_resize_internal and _vec_resize_internal returns a valid
        // pointer or aborts, and the pointer has memory laid out as we expect, i.e. the
        // vec_header_t before it, and we preserve len to avoid uninitialised element access.
        unsafe {
            let ptr = _vec_resize_internal(self.as_mut_ptr().cast(), new_len.into(), &attr);
            self.0 = NonNull::new_unchecked(ptr.cast());
            // Preserve len, since _vec_resize_internal adds to it, whereas it's unallocated space for us
            self.set_len(len);
        }
    }

    /// Pushes an element to the end of the vector
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `u32`.
    pub fn push(&mut self, value: T) {
        let len = self.len();
        if len == self.capacity() {
            self.reserve(1);
        }
        // SAFETY: creation preconditions mean this is a valid object, and we have reserved
        // space for at least one more element and len is set correctly, with grow_elts
        // updated so the capacity is correct.
        unsafe {
            let end = self.as_mut_ptr().add(len);
            ptr::write(end, value);
            let hdr = self.as_vec_header_ptr();
            (*hdr).len = len as u32 + 1;
            (*hdr).grow_elts -= 1;
        }
    }

    /// Inserts an element at the specified index, shifting all elements after it to the right
    ///
    /// # Panics
    ///
    /// Panics if the new capacity overflows `u32`.
    pub fn insert_mut(&mut self, index: usize, element: T) -> &mut T {
        let len = self.len();
        let capacity = self.capacity();

        assert!(
            index < len,
            "insertion index (is {index}) should be <= len (is {len})"
        );

        if len == capacity {
            self.reserve(1);
        }
        // SAFETY: creation preconditions mean this is a valid object, index is valid and we have
        // reserved space for at least one more element and len is set correctly, with grow_elts
        // updated so the capacity is correct, and the returned reference is valid until the next
        // mutation of the vector.
        unsafe {
            let p = self.as_mut_ptr().add(index);
            if index < len {
                // Shift everything over to make space. (Duplicating the
                // `index`th element into two consecutive places.)
                ptr::copy(p, p.add(1), len - index);
            }
            ptr::write(p, element);
            let hdr = self.as_vec_header_ptr();
            (*hdr).len = len as u32 + 1;
            (*hdr).grow_elts -= 1;
            &mut *p
        }
    }

    /// Consumes the vector, returning a raw pointer to the first element
    ///
    /// After calling this method, the caller is responsible for managing the memory of the
    /// vector. In particular, the caller should call the destructor (if any) for each element
    /// and free the memory allocated for the vector.
    #[must_use = "losing the pointer will leak the vector"]
    pub fn into_raw(self) -> *mut T {
        let v = ManuallyDrop::new(self);
        v.as_mut_ptr()
    }

    /// Returns a raw pointer to the first element of the vector
    ///
    /// The caller must ensure that the vector outlives the returned pointer. Modifying the vector
    /// may invalidate the pointer.
    ///
    /// The caller must ensure that the pointer is not used to mutate the vector (except inside an
    /// `UnsafeCell`).
    ///
    /// If you need a mutable pointer, use [`Self::as_mut_ptr`] instead.
    #[inline(always)]
    pub const fn as_ptr(&self) -> *const T {
        self.0.as_ptr()
    }

    /// Returns a raw pointer to the first element of the vector
    ///
    /// The caller must ensure that the vector outlives the returned pointer. Modifying the vector
    /// may invalidate the pointer.
    ///
    /// The caller must ensure that the pointer is not used to mutate the vector (except inside an
    /// `UnsafeCell`).
    ///
    /// If you need a mutable pointer, use [`Self::as_mut_ptr`] instead.
    #[inline(always)]
    pub const fn as_mut_ptr(&self) -> *mut T {
        self.0.as_ptr()
    }
}

impl<T> Default for Vec<T> {
    fn default() -> Vec<T> {
        Vec::new()
    }
}

impl<T: Clone> Vec<T> {
    /// Extend the vector by `n` clones of value.
    pub fn extend_with(&mut self, n: usize, value: T) {
        self.reserve(n);
        let new_len = self.len() + n;

        // SAFETY: creation preconditions mean this is a valid object, we have reserved
        // space for n more elements, the new elements are initialised by writing the value and
        // set_len is called with the correct new length.
        unsafe {
            let mut ptr = self.as_mut_ptr().add(self.len());

            // Write all elements except the last one
            for _ in 1..n {
                ptr::write(ptr, value.clone());
                ptr = ptr.add(1);
            }

            if n > 0 {
                // We can write the last element directly without cloning needlessly
                ptr::write(ptr, value);
            }

            self.set_len(new_len);
        }
    }
}

impl<T> Extend<T> for Vec<T> {
    #[track_caller]
    fn extend<I: IntoIterator<Item = T>>(&mut self, iter: I) {
        let mut iter = iter.into_iter();
        while let Some(element) = iter.next() {
            let len = self.len();
            if len == self.capacity() {
                let (lower, _) = iter.size_hint();
                self.reserve(lower.saturating_add(1));
            }
            // SAFETY: creation preconditions mean this is a valid object, and we have reserved
            // space for at least one more element the new element is initialised by writing the
            // value and set_len is called with the correct new length.
            unsafe {
                ptr::write(self.as_mut_ptr().add(len), element);
                self.set_len(len + 1);
            }
        }
    }
}

impl<'a, T: Copy + 'a> Extend<&'a T> for Vec<T> {
    #[track_caller]
    fn extend<I: IntoIterator<Item = &'a T>>(&mut self, iter: I) {
        let mut iter = iter.into_iter();
        while let Some(element) = iter.next() {
            let len = self.len();
            if len == self.capacity() {
                let (lower, _) = iter.size_hint();
                self.reserve(lower.saturating_add(1));
            }
            // SAFETY: creation preconditions mean this is a valid object, and we have reserved
            // space for at least one more element the new element is initialised by writing the
            // value and set_len is called with the correct new length.
            unsafe {
                ptr::write(self.as_mut_ptr().add(len), *element);
                self.set_len(len + 1);
            }
        }
    }
}

impl<T> Drop for Vec<T> {
    fn drop(&mut self) {
        let elems: *mut [T] = self.as_mut_slice();
        // SAFETY: creation preconditions mean this is a valid object, and on exit the memory is
        // freed so the dropped elements cannot be accessed again
        unsafe {
            ptr::drop_in_place(elems);
            vec_free_not_inline(self.as_mut_ptr().cast())
        }
    }
}

impl<T> Deref for Vec<T> {
    type Target = VecRef<T>;

    fn deref(&self) -> &VecRef<T> {
        // SAFETY: creation preconditions mean this is a valid object and is compatible with VecRef<T>
        unsafe { VecRef::from_raw(self.as_ptr()) }
    }
}

impl<T> DerefMut for Vec<T> {
    fn deref_mut(&mut self) -> &mut VecRef<T> {
        // SAFETY: creation preconditions mean this is a valid object and is compatible with VecRef<T>
        unsafe { VecRef::from_raw_mut(self.as_mut_ptr()) }
    }
}

impl<T: Clone> Clone for Vec<T> {
    fn clone(&self) -> Self {
        self.as_slice().to_vpp_vec()
    }
}

impl<T: fmt::Debug> fmt::Debug for Vec<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_slice(), f)
    }
}

impl<T> AsRef<Vec<T>> for Vec<T> {
    fn as_ref(&self) -> &Vec<T> {
        self
    }
}

impl<T> AsMut<Vec<T>> for Vec<T> {
    fn as_mut(&mut self) -> &mut Vec<T> {
        self
    }
}

impl<T> AsRef<[T]> for Vec<T> {
    fn as_ref(&self) -> &[T] {
        self
    }
}

impl<T> AsMut<[T]> for Vec<T> {
    fn as_mut(&mut self) -> &mut [T] {
        self
    }
}

impl<T> Borrow<VecRef<T>> for Vec<T> {
    fn borrow(&self) -> &VecRef<T> {
        // SAFETY: creation preconditions mean this is a valid object and is compatible with VecRef<T>
        unsafe { VecRef::from_raw(self.as_ptr()) }
    }
}

impl<T> BorrowMut<VecRef<T>> for Vec<T> {
    fn borrow_mut(&mut self) -> &mut VecRef<T> {
        // SAFETY: creation preconditions mean this is a valid object and is compatible with VecRef<T>
        unsafe { VecRef::from_raw_mut(self.as_mut_ptr()) }
    }
}

impl<T> PartialOrd<Vec<T>> for Vec<T>
where
    T: PartialOrd,
{
    #[inline]
    fn partial_cmp(&self, other: &Vec<T>) -> Option<Ordering> {
        PartialOrd::partial_cmp(&**self, &**other)
    }
}

impl<T: Eq> Eq for Vec<T> {}

impl<T: Ord> Ord for Vec<T> {
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(&**self, &**other)
    }
}

/// Extension trait for slices to convert to VPP vectors
pub trait SliceExt<T> {
    /// Converts the slice to a VPP vector by cloning each element
    fn to_vpp_vec(&self) -> Vec<T>
    where
        T: Clone;
}

impl<T> SliceExt<T> for [T] {
    fn to_vpp_vec(&self) -> Vec<T>
    where
        T: Clone,
    {
        let len = self.len();
        let mut vec = Vec::with_capacity(len);
        let slots = vec.spare_capacity_mut();
        // .take(slots.len()) is necessary for LLVM to remove bounds checks
        // and has better codegen than zip.
        for (i, b) in self.iter().enumerate().take(slots.len()) {
            slots[i].write(b.clone());
        }
        // SAFETY:
        // the vec was allocated and initialized above to at least this length.
        unsafe {
            vec.set_len(self.len());
        }
        vec
    }
}

macro_rules! __impl_slice_eq {
    ([$($vars:tt)*] $lhs:ty, $rhs:ty $(where $ty:ty: $bound:ident)?) => {
        impl<T, U, $($vars)*> PartialEq<$rhs> for $lhs
        where
            T: PartialEq<U>,
            $($ty: $bound)?
        {
            #[inline]
            fn eq(&self, other: &$rhs) -> bool { self[..] == other[..] }
        }
    }
}

__impl_slice_eq! { [] VecRef<T>, VecRef<U> }
__impl_slice_eq! { [] VecRef<T>, std::vec::Vec<U> }
__impl_slice_eq! { [] VecRef<T>, &[U] }
__impl_slice_eq! { [] VecRef<T>, &mut [U] }
__impl_slice_eq! { [] VecRef<T>, [U] }
__impl_slice_eq! { [] VecRef<T>, Vec<U> }
__impl_slice_eq! { [] Vec<T>, Vec<U> }
__impl_slice_eq! { [] Vec<T>, VecRef<U> }
__impl_slice_eq! { [] Vec<T>, std::vec::Vec<U> }
__impl_slice_eq! { [] Vec<T>, &[U] }
__impl_slice_eq! { [] Vec<T>, &mut [U] }
__impl_slice_eq! { [] Vec<T>, [U] }
__impl_slice_eq! { [const N: usize] VecRef<T>, [U; N] }
__impl_slice_eq! { [const N: usize] VecRef<T>, &[U; N] }

// SAFETY: it's fine to deallocate the memory from another thread, and fine to access Vec<T> from
// another thread as long as T is Send
unsafe impl<T> Send for Vec<T> where T: Send {}

/// Creates a [`Vec<T>`] from a list of elements
///
/// Allows creating a VPP vector in a similar way to the standard library's `vec!` macro.
#[macro_export]
macro_rules! vec {
    () => (
        $crate::vppinfra::vec::Vec::new()
    );
    ($elem:expr; $n:expr) => ({
        let mut v = $crate::vppinfra::vec::Vec::with_capacity($n);
        v.extend_with($n, $elem);
        v
    });
    ($($x:expr),+ $(,)?) => (
        $crate::vppinfra::vec::SliceExt::to_vpp_vec(
            [$($x),+].as_slice()
        )
    );
}

#[cfg(test)]
mod tests {
    use crate::{vec, vppinfra::clib_mem_init};

    use super::Vec;

    #[test]
    fn push_pop() {
        clib_mem_init();

        let mut v = Vec::new();
        v.push(1);
        v.push(2);
        v.push(3);
        assert_eq!(v.len(), 3);
        assert!(v.capacity() >= 3);

        assert_eq!(v.pop(), Some(3));
        assert_eq!(v.pop(), Some(2));
        assert_eq!(v.pop(), Some(1));
        assert_eq!(v.pop(), None);
    }

    #[test]
    fn test_macro() {
        clib_mem_init();

        let v: Vec<u8> = vec![];
        assert_eq!(v.len(), 0);

        let v = vec![1, 2, 3];
        assert_eq!(v[0], 1);
        assert_eq!(v[1], 2);
        assert_eq!(v[2], 3);

        let v = vec![1; 3];
        assert_eq!(v[0], 1);
        assert_eq!(v[1], 1);
        assert_eq!(v[2], 1);
    }

    #[test]
    fn extend_from() {
        clib_mem_init();

        let mut v: Vec<u32> = Vec::new();
        v.extend(&[1, 2, 3]);
        assert_eq!(v.len(), 3);
        assert_eq!(v[0], 1);
        assert_eq!(v[1], 2);
        assert_eq!(v[2], 3);
    }
}
