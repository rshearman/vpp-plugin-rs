//! Cache utilities and definitions
//!
//! This module provides functions for prefetching memory locations into CPU caches, and
//! markers for aligning types to cache lines or other power-of-two boundaries.

use std::{
    fmt,
    hash::{Hash, Hasher},
};

use crate::bindings::CLIB_LOG2_CACHE_LINE_BYTES;

/// Size of a CPU cache line in bytes, as configured for the VPP build
pub const CACHE_LINE_BYTES: usize = 1 << CLIB_LOG2_CACHE_LINE_BYTES;

/// Prefetches a memory location into L1 cache for reading
// Instead of std::intrinsics::prefetch_read_data which is nightly-only
#[inline(always)]
pub fn prefetch_load<T>(p: *const T) {
    // Suppress unused variable warning on other platforms
    let _ = p;

    #[cfg(target_arch = "x86_64")]
    // SAFETY: it's not clear why this is marked as unsafe, as prefetching a pointer
    // doesn't dereference it or read from it.
    unsafe {
        std::arch::x86_64::_mm_prefetch::<{ std::arch::x86_64::_MM_HINT_T0 }>(p as *const i8);
    }
}

/// Prefetches a memory location into L1 cache for writing
// Instead of std::intrinsics::prefetch_write_data which is nightly-only
#[inline(always)]
pub fn prefetch_store<T>(p: *const T) {
    // Suppress unused variable warning on other platforms
    let _ = p;

    #[cfg(target_arch = "x86_64")]
    // SAFETY: it's not clear why this is marked as unsafe, as prefetching a pointer
    // doesn't dereference it or write to it.
    unsafe {
        std::arch::x86_64::_mm_prefetch::<{ std::arch::x86_64::_MM_HINT_ET0 }>(p as *const i8);
    }
}

/// Trait indicating that [`AlignMarker<N>`] is a valid alignment
///
/// This is implemented for [`AlignMarker<N>`] for every `N` that is a power of two from 1 to
/// 2<sup>29</sup> inclusive, which is the same set of values accepted by `#[repr(align(N))]`.
/// It exists so that an invalid alignment is rejected at compile time:
///
/// ```compile_fail
/// # use vpp_plugin::vppinfra::cache::AlignMarker;
/// let _ = AlignMarker::<3>::new();
/// ```
pub trait ValidAlign {
    /// Zero-sized type with an alignment of `N`
    #[doc(hidden)]
    type Repr: Copy;
}

/// Zero-sized marker that raises the alignment of the type containing it to `N` bytes
///
/// Adding a field of this type to a struct has the same effect on layout as annotating the
/// struct with `#[repr(align(N))]`: the struct's alignment becomes at least `N` and its size is
/// rounded up to a multiple of `N`. The marker itself occupies no space. The generated code is
/// also identical, as is the ABI, to a struct annotated with `#[repr(align(N))]`.
///
/// The advantage over `#[repr(align(N))]` is that `N` can be any constant expression, rather
/// than only an integer literal.
///
/// To raise the alignment to a cache line, instead use [`CacheLineAlignMarker`].
///
/// # Examples
///
/// ```
/// use vpp_plugin::vppinfra::cache::AlignMarker;
///
/// #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, Hash)]
/// struct Aligned {
///     _align: AlignMarker<32>,
///     value: u64,
/// }
///
/// assert_eq!(std::mem::align_of::<Aligned>(), 32);
/// assert_eq!(std::mem::size_of::<Aligned>(), 32);
///
/// static ALIGNED: Aligned = Aligned {
///     _align: AlignMarker::new(),
///     value: 42,
/// };
/// assert_eq!(&raw const ALIGNED as usize % 32, 0);
/// ```
#[repr(transparent)]
#[derive(Default, Clone, Copy)]
pub struct AlignMarker<const N: usize>([<Self as ValidAlign>::Repr; 0])
where
    Self: ValidAlign;

impl<const N: usize> AlignMarker<N>
where
    Self: ValidAlign,
{
    /// Creates a new alignment marker
    pub const fn new() -> Self {
        Self([])
    }
}

impl<const N: usize> fmt::Debug for AlignMarker<N>
where
    Self: ValidAlign,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "AlignMarker<{N}>")
    }
}

// The following are implemented manually rather than derived for two reasons: deriving would
// require adding the trait as a bound on `ValidAlign::Repr`, and derived `Hash` would hash the
// length of the zero-length array rather than doing nothing.

impl<const N: usize> PartialEq for AlignMarker<N>
where
    Self: ValidAlign,
{
    #[inline]
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<const N: usize> Eq for AlignMarker<N> where Self: ValidAlign {}

impl<const N: usize> PartialOrd for AlignMarker<N>
where
    Self: ValidAlign,
{
    #[inline]
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<const N: usize> Ord for AlignMarker<N>
where
    Self: ValidAlign,
{
    #[inline]
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

impl<const N: usize> Hash for AlignMarker<N>
where
    Self: ValidAlign,
{
    #[inline]
    fn hash<H: Hasher>(&self, _state: &mut H) {}
}

/// Zero-sized types used to give [`AlignMarker`] its alignment
///
/// These are `pub` so that they can be used in the public [`ValidAlign`] trait, but they live in
/// a private module so they can't be named outside of this module.
mod repr {
    use super::{AlignMarker, ValidAlign};

    macro_rules! impl_valid_align {
        ($($repr:ident = $n:literal),* $(,)?) => {
            $(
                #[derive(Clone, Copy)]
                #[repr(align($n))]
                pub struct $repr;

                impl ValidAlign for AlignMarker<$n> {
                    type Repr = $repr;
                }

                const _: () = {
                    assert!(std::mem::size_of::<AlignMarker<$n>>() == 0);
                    assert!(std::mem::align_of::<AlignMarker<$n>>() == $n);
                };
            )*
        };
    }

    // `#[repr(align(N))]` accepts powers of two up to and including 2^29
    impl_valid_align! {
        Align1 = 1,
        Align2 = 2,
        Align4 = 4,
        Align8 = 8,
        Align16 = 16,
        Align32 = 32,
        Align64 = 64,
        Align128 = 128,
        Align256 = 256,
        Align512 = 512,
        Align1K = 1024,
        Align2K = 2048,
        Align4K = 4096,
        Align8K = 8192,
        Align16K = 16384,
        Align32K = 32768,
        Align64K = 65536,
        Align128K = 131072,
        Align256K = 262144,
        Align512K = 524288,
        Align1M = 1048576,
        Align2M = 2097152,
        Align4M = 4194304,
        Align8M = 8388608,
        Align16M = 16777216,
        Align32M = 33554432,
        Align64M = 67108864,
        Align128M = 134217728,
        Align256M = 268435456,
        Align512M = 536870912,
    }
}

/// Marker that aligns the type containing it to the size of a cache line
///
/// This is equivalent to VPP's `CLIB_CACHE_LINE_ALIGN_MARK`. See [`AlignMarker`] for details.
pub type CacheLineAlignMarker = AlignMarker<CACHE_LINE_BYTES>;

#[cfg(test)]
mod tests {
    use std::{
        collections::hash_map::DefaultHasher,
        mem::{align_of, size_of},
    };

    use super::*;
    use crate::const_assert;

    #[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
    struct CacheAlignedStruct {
        _align: CacheLineAlignMarker,
        values: [u64; 2],
    }

    #[test]
    fn cache_aligned_struct_layout() {
        const_assert!(size_of::<CacheAlignedStruct>() == CACHE_LINE_BYTES);
        const_assert!(align_of::<CacheAlignedStruct>() == CACHE_LINE_BYTES);

        let t = CacheAlignedStruct::default();
        assert_eq!(t.values, [0, 0]);
        assert_eq!(&raw const t as usize % CACHE_LINE_BYTES, 0);

        let boxed = Box::new(CacheAlignedStruct::default());
        assert_eq!(&raw const *boxed as usize % CACHE_LINE_BYTES, 0);

        let v = vec![CacheAlignedStruct::default(); 3];
        for e in &v {
            assert_eq!(e as *const _ as usize % CACHE_LINE_BYTES, 0);
        }
    }

    #[test]
    fn same_layout_as_repr_align() {
        #[repr(align(64))]
        struct ReprAlign {
            _a: u8,
            _b: u32,
        }
        struct Marker {
            _align: AlignMarker<64>,
            _a: u8,
            _b: u32,
        }
        const_assert!(size_of::<ReprAlign>() == size_of::<Marker>());
        const_assert!(align_of::<ReprAlign>() == align_of::<Marker>());
        const_assert!(size_of::<Option<ReprAlign>>() == size_of::<Option<Marker>>());
    }

    #[test]
    fn repr_c_layout() {
        #[repr(C)]
        struct S {
            _align: AlignMarker<16>,
            a: u8,
            b: u32,
        }
        const_assert!(size_of::<S>() == 16);
        const_assert!(align_of::<S>() == 16);
        const_assert!(std::mem::offset_of!(S, a) == 0);
        const_assert!(std::mem::offset_of!(S, b) == 4);
    }

    #[test]
    fn marker_does_not_reduce_alignment() {
        struct S {
            _align: AlignMarker<1>,
            _a: u64,
        }
        const_assert!(align_of::<S>() == align_of::<u64>());
        const_assert!(size_of::<S>() == size_of::<u64>());
    }

    #[test]
    fn max_alignment() {
        // Only check the layout; don't allocate 512 MiB
        struct S {
            _align: AlignMarker<{ 1 << 29 }>,
            _a: u8,
        }
        const_assert!(align_of::<S>() == 1 << 29);
        const_assert!(size_of::<S>() == 1 << 29);
    }

    #[test]
    fn const_construction() {
        static S: CacheAlignedStruct = CacheAlignedStruct {
            _align: AlignMarker::new(),
            values: [1, 2],
        };
        assert_eq!(&raw const S as usize % CACHE_LINE_BYTES, 0);
        assert_eq!(S.values, [1, 2]);
    }

    #[test]
    fn traits() {
        let a = AlignMarker::<8>::new();
        let b = AlignMarker::<8>::default();
        assert_eq!(a, b);
        assert_eq!(a.cmp(&b), std::cmp::Ordering::Equal);
        assert_eq!(a.partial_cmp(&b), Some(std::cmp::Ordering::Equal));
        assert_eq!(format!("{a:?}"), "AlignMarker<8>");

        // Hashing the marker should be a no-op so that it doesn't affect the hash of the
        // containing type
        let mut h1 = DefaultHasher::new();
        let mut h2 = DefaultHasher::new();
        a.hash(&mut h1);
        assert_eq!(h1.finish(), h2.finish());
        let s1 = CacheAlignedStruct {
            _align: AlignMarker::new(),
            values: [3, 4],
        };
        s1.hash(&mut h1);
        s1.values.hash(&mut h2);
        assert_eq!(h1.finish(), h2.finish());

        // Comparisons on the containing type should only depend on the other fields
        let s2 = CacheAlignedStruct {
            _align: AlignMarker::new(),
            values: [3, 5],
        };
        assert!(s1 < s2);
        assert_ne!(s1, s2);

        fn assert_auto_traits<T: Send + Sync + Unpin + std::panic::UnwindSafe>() {}
        assert_auto_traits::<CacheLineAlignMarker>();
    }
}
