//! Core synchronization primitives for vlib main.

use std::{
    cell::{Cell, UnsafeCell},
    fmt,
    marker::PhantomData,
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::vlib::{BarrierHeldMainRef, MainRef};

/// A read/write lock using VPP's barrier to provide exclusion between threads
///
/// VPP implements a barrier in the main thread which blocks all worker threads from running. The
/// `BarrierRwLock` is an abstraction around this which allows a writer in the VPP main thread
/// whilst the barrier is held and readers in either VPP workers or the VPP main thread.
///
/// Taking read or write "locks" are guaranteed to never block - blocking instead occurs in the
/// VPP main and worker threads when the VPP barrier is taken.
pub struct BarrierRwLock<T: ?Sized> {
    /// The number of readers
    ///
    /// Note that this doesn't use `AtomicU32` because it's only modified on the VPP main thread.
    readers: UnsafeCell<u32>,
    /// Whether there is a writer
    ///
    /// Note that this doesn't use `AtomicBool` because it's only modified on the VPP main thread.
    writer: UnsafeCell<bool>,
    /// The lock-protected data.
    data: UnsafeCell<T>,
}

impl<T> BarrierRwLock<T> {
    /// Create a new barrier-backed read/write lock.
    #[inline]
    pub const fn new(t: T) -> Self {
        Self {
            data: UnsafeCell::new(t),
            readers: UnsafeCell::new(0),
            writer: UnsafeCell::new(false),
        }
    }
}

impl<T: ?Sized> BarrierRwLock<T> {
    /// Locks this `BarrierRwLock` with shared read access.
    ///
    /// Returns an RAII guard which will release this thread's shared access
    /// once it is dropped.
    ///
    /// # Panics
    ///
    /// Panics if a write lock has already been taken by this thread and not dropped.
    #[inline(always)]
    pub fn read<'a>(&'a self, vm: &'a MainRef) -> BarrierRwLockReadGuard<'a, T> {
        let main_thread = vm.thread_index() == 0;
        // SAFETY: calling `BarrierRwLockReadGuard::new` is valid when we have a reference to the lock
        // and we are on a known VPP thread. These conditions are satisfied by the public API.
        unsafe { BarrierRwLockReadGuard::new(self, main_thread) }
    }

    /// Locks this `BarrierRwLock` with write access.
    ///
    /// This is used on the VPP main thread in contexts where the VPP barrier is held.
    ///
    /// Returns an RAII guard which will release this thread's access
    /// once it is dropped.
    ///
    /// # Panics
    ///
    /// Panics if a read or another write lock has already been taken by this thread and not
    /// dropped.
    #[inline(always)]
    pub fn write<'a>(&'a self, vm: &'a BarrierHeldMainRef) -> BarrierRwLockWriteGuard<'a, T> {
        // Make sure we match the check in read()
        debug_assert_eq!(vm.thread_index(), 0);
        // SAFETY: `BarrierRwLockWriteGuard::new` is only called on the main thread while the
        // barrier is held.
        unsafe { BarrierRwLockWriteGuard::new(self) }
    }

    /// Get a mutable reference to the contained data without locking.
    ///
    /// This call borrows the `BarrierRwLock` mutably (at compile-time) which guarantees that we
    /// possess the only reference.
    pub fn get_mut(&mut self) -> &mut T {
        self.data.get_mut()
    }

    /// Returns a raw pointer to the underlying data.
    ///
    /// The returned pointer is always non-null and properly aligned, but it is
    /// the user's responsibility to ensure that any reads and writes through it
    /// are properly synchronized to avoid data races, and that it is not read
    /// or written through after the lock is dropped.
    pub const fn data_ptr(&self) -> *mut T {
        self.data.get()
    }
}

impl<T> BarrierRwLock<T> {
    /// Consume the lock and return the underlying data.
    pub fn into_inner(self) -> T {
        self.data.into_inner()
    }
}

// SAFETY: `BarrierRwLock<T>` is safe to send to another thread if `T: Send`.
unsafe impl<T: ?Sized + Send> Send for BarrierRwLock<T> {}

// SAFETY: `BarrierRwLock<T>` is safe to share between threads if `T: Send + Sync`.
unsafe impl<T: ?Sized + Send + Sync> Sync for BarrierRwLock<T> {}

impl<T: Default> Default for BarrierRwLock<T> {
    /// Creates a new `BarrierRwLock<T>`, with the `Default` value for T.
    fn default() -> BarrierRwLock<T> {
        BarrierRwLock::new(Default::default())
    }
}

/// Shared read guard returned by [`BarrierRwLock::read`].
pub struct BarrierRwLockReadGuard<'rwlock, T: ?Sized + 'rwlock> {
    /// A pointer to the data protected by the `BarrierRwLock`. Note that we use a pointer here
    /// instead of `&'rwlock T` to avoid `noalias` violations, because a `BarrierRwLockReadGuard`
    /// instance only holds immutability until it drops, not for its whole scope.
    data: NonNull<T>,

    /// A reference to the [`BarrierRwLock`] that we have read-locked.
    lock: &'rwlock BarrierRwLock<T>,

    /// Whether the lock is on the VPP main thread or not
    main_thread: bool,
}

// Note: Send not implemented here as that would prevent the optimisation of not incrementing
// readers for VPP worker threads, since the guard could then be sent to the VPP main thread
// and used to access data while there is a write lock taken, which violates `noalias` rules.

// SAFETY: `BarrierRwLockReadGuard` is immutable references to valid data; `Sync` is safe for T: Sync.
unsafe impl<T: ?Sized + Sync> Sync for BarrierRwLockReadGuard<'_, T> {}

impl<'rwlock, T: ?Sized> BarrierRwLockReadGuard<'rwlock, T> {
    /// Creates a new instance of `BarrierRwLockReadGuard<T>` from a `BarrierRwLock<T>`.
    ///
    /// # Panics
    ///
    /// Panics if a write lock has already been taken by this thread and not dropped.
    ///
    /// # Safety
    ///
    /// This function is safe if and only if called from a thread that VPP barriers know about,
    /// i.e. either the VPP main thread or a VPP worker thread.
    #[inline(always)]
    unsafe fn new(
        lock: &'rwlock BarrierRwLock<T>,
        main_thread: bool,
    ) -> BarrierRwLockReadGuard<'rwlock, T> {
        // SAFETY: `lock.writer` is valid because `lock` is a valid pointer to a live lock.
        if main_thread && unsafe { *lock.writer.get() } {
            panic!("Write lock already taken by this thread");
        }

        // SAFETY: `lock.data` is valid and aligned, and lock lifetime guarantees it outlives the guard.
        let data = unsafe { NonNull::new_unchecked(lock.data.get()) };
        if main_thread {
            // SAFETY: Only main thread increments/decrements readers so there is no data race.
            unsafe {
                *lock.readers.get() += 1;
            }
        }
        Self {
            data,
            lock,
            main_thread,
        }
    }
}

impl<T: ?Sized> Drop for BarrierRwLockReadGuard<'_, T> {
    #[inline(always)]
    fn drop(&mut self) {
        if self.main_thread {
            // SAFETY: Only main thread mutates `readers` so there is no data race. We are on
            // the main thread by conditional.
            unsafe {
                *self.lock.readers.get() -= 1;
            }
        }
    }
}

impl<T: ?Sized> Deref for BarrierRwLockReadGuard<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        // SAFETY: the conditions of `BarrierRwLockReadGuard::new` were satisfied when created.
        unsafe { self.data.as_ref() }
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for BarrierRwLockReadGuard<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for BarrierRwLockReadGuard<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

/// Exclusive write guard returned by [`BarrierRwLock::write`].
pub struct BarrierRwLockWriteGuard<'rwlock, T: ?Sized + 'rwlock> {
    /// A reference to the [`RwLock`] that we have write-locked.
    lock: &'rwlock BarrierRwLock<T>,

    /// Prevent the type from being Send
    _phantom: PhantomData<Cell<()>>,
}

impl<'rwlock, T: ?Sized> BarrierRwLockWriteGuard<'rwlock, T> {
    /// Creates a new instance of `BarrierRwLockWriteGuard<T>` from a `BarrierRwLock<T>`.
    ///
    /// # Panics
    ///
    /// Panics if a read or another write lock has already been taken by this thread and not
    /// dropped.
    ///
    /// # Safety
    ///
    /// This function is safe if and only if the same thread is holding the VPP barrier prior to
    /// calling this function and continues to hold it for the lifetime of this object.
    #[inline(always)]
    unsafe fn new(lock: &'rwlock BarrierRwLock<T>) -> BarrierRwLockWriteGuard<'rwlock, T> {
        // SAFETY: this function is only called with barrier held and no concurrent write.
        unsafe {
            if *lock.readers.get() != 0 {
                panic!("Read lock already taken by this thread");
            }
            if *lock.writer.get() {
                panic!("Write lock already taken by this thread");
            }
            *lock.writer.get() = true;
        }
        BarrierRwLockWriteGuard {
            lock,
            _phantom: PhantomData,
        }
    }
}

impl<T: ?Sized> Drop for BarrierRwLockWriteGuard<'_, T> {
    #[inline(always)]
    fn drop(&mut self) {
        // SAFETY: This is the only writer and barrier is held while the guard is alive.
        unsafe {
            *self.lock.writer.get() = false;
        }
    }
}

// Note: no Send implementation as it's not safe to modify `self.lock.writer` on Drop and sending
// the write guard across threads has limited usefulness.

// SAFETY: `BarrierRwLockWriteGuard` ensures exclusive write access to the protected data
// during its lifetime via VPP's barrier mechanism, which prevents concurrent access from
// worker threads. For `T: Sync`, the guard can be safely shared across threads because
// the underlying data is `Sync` and the barrier guarantees no conflicting accesses occur.
unsafe impl<T: ?Sized + Sync> Sync for BarrierRwLockWriteGuard<'_, T> {}

impl<T: ?Sized> Deref for BarrierRwLockWriteGuard<'_, T> {
    type Target = T;

    #[inline(always)]
    fn deref(&self) -> &T {
        // SAFETY: the conditions of `BarrierRwLockWriteGuard::new` were satisfied when created.
        unsafe { &*self.lock.data.get() }
    }
}

impl<T: ?Sized> DerefMut for BarrierRwLockWriteGuard<'_, T> {
    #[inline(always)]
    fn deref_mut(&mut self) -> &mut T {
        // SAFETY: the conditions of `BarrierRwLockWriteGuard::new` were satisfied when created.
        unsafe { &mut *self.lock.data.get() }
    }
}

impl<T: ?Sized + fmt::Debug> fmt::Debug for BarrierRwLockWriteGuard<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

impl<T: ?Sized + fmt::Display> fmt::Display for BarrierRwLockWriteGuard<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        (**self).fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use std::thread;

    use crate::{
        bindings::vlib_main_t,
        vlib::{BarrierHeldMainRef, MainRef, main::sync::BarrierRwLock},
    };

    #[test]
    fn concurrent_reads() {
        let lock = BarrierRwLock::new("value".to_string());
        let ref_lock = &lock;
        thread::scope(|s| {
            let thread1 = s.spawn(move || {
                let mut main = vlib_main_t::default();
                // SAFETY: main is sufficiently initialised for the test and valid for the duration of the
                // call.
                let main_ref = unsafe { MainRef::from_ptr_mut(std::ptr::addr_of_mut!(main)) };
                for _ in 0..1000 {
                    assert_eq!(*ref_lock.read(main_ref), "value");
                }
            });
            let thread2 = s.spawn(move || {
                let mut main = vlib_main_t {
                    thread_index: 1,
                    ..vlib_main_t::default()
                };
                // SAFETY: main is sufficiently initialised for the test and valid for the duration of the
                // call.
                let main_ref = unsafe { MainRef::from_ptr_mut(std::ptr::addr_of_mut!(main)) };
                for _ in 0..1000 {
                    assert_eq!(*ref_lock.read(main_ref), "value");
                }
            });
            thread1.join().unwrap();
            thread2.join().unwrap();
        });
    }

    #[test]
    fn write_guard() {
        let mut main = vlib_main_t::default();
        // SAFETY: main is sufficiently initialised for the test and valid for the duration of the
        // call.
        let main_ref = unsafe { BarrierHeldMainRef::from_ptr_mut(std::ptr::addr_of_mut!(main)) };
        let lock = BarrierRwLock::new("value".to_string());
        *lock.write(main_ref) = "new value".to_string();
        assert_eq!(*lock.read(main_ref), "new value");
    }

    #[test]
    #[should_panic(expected = "Write lock already taken by this thread")]
    fn read_and_write1() {
        let mut main = vlib_main_t::default();
        // SAFETY: main is sufficiently initialised for the test and valid for the duration of the
        // call.
        let main_ref = unsafe { BarrierHeldMainRef::from_ptr_mut(std::ptr::addr_of_mut!(main)) };
        let lock = BarrierRwLock::new("value".to_string());
        let _guard1 = lock.write(main_ref);
        let _guard2 = lock.read(main_ref);
    }

    #[test]
    #[should_panic(expected = "Read lock already taken by this thread")]
    fn read_and_write2() {
        let mut main = vlib_main_t::default();
        // SAFETY: main is sufficiently initialised for the test and valid for the duration of the
        // call.
        let main_ref = unsafe { BarrierHeldMainRef::from_ptr_mut(std::ptr::addr_of_mut!(main)) };
        let lock = BarrierRwLock::new("value".to_string());
        let _guard1 = lock.read(main_ref);
        let _guard2 = lock.write(main_ref);
    }

    #[test]
    #[should_panic(expected = "Write lock already taken by this thread")]
    fn write_write() {
        let mut main = vlib_main_t::default();
        // SAFETY: main is sufficiently initialised for the test and valid for the duration of the
        // call.
        let main_ref = unsafe { BarrierHeldMainRef::from_ptr_mut(std::ptr::addr_of_mut!(main)) };
        let lock = BarrierRwLock::new("value".to_string());
        let _guard1 = lock.write(main_ref);
        let _guard2 = lock.write(main_ref);
    }

    /// Test misc small utilities of [`BarrierRwLock`]
    #[test]
    fn misc() {
        let mut main = vlib_main_t::default();
        // SAFETY: main is sufficiently initialised for the test and valid for the duration of the
        // call.
        let main_ref = unsafe { BarrierHeldMainRef::from_ptr_mut(std::ptr::addr_of_mut!(main)) };
        let mut lock: BarrierRwLock<String> = BarrierRwLock::default();

        assert_eq!(*lock.write(main_ref), "");

        *lock.get_mut() = "value".to_string();

        assert_eq!(lock.write(main_ref).to_string(), "value");
        assert_eq!(format!("{:?}", lock.write(main_ref)), "\"value\"");
        assert_eq!(lock.read(main_ref).to_string(), "value");
        assert_eq!(format!("{:?}", lock.read(main_ref)), "\"value\"");

        // SAFETY: data_ptr() returns a valid pointer and it remains valid throughout its use
        unsafe {
            assert_eq!(&*lock.data_ptr(), "value");
        }

        assert_eq!(lock.into_inner(), "value");
    }
}
