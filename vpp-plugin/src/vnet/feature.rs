//! VPP networking features

use std::{cell::UnsafeCell, marker::PhantomData};

use crate::{
    bindings::{
        feature_main, vlib_helper_remove_feature_from_registrations, vnet_feature_enable_disable,
        vnet_feature_registration_t,
    },
    vlib::{
        node::{Node, NodeRegistration},
        BarrierHeldMainRef,
    },
    vnet::{error::VnetError, types::SwIfIndex},
};

/// Registration information for a VPP feature
///
/// This is typically created automatically using the [`crate::vnet_feature_init`] macro.
pub struct FeatureRegistration<FeatureConfig> {
    feat: UnsafeCell<vnet_feature_registration_t>,
    _marker: PhantomData<FeatureConfig>,
}

impl<FeatureConfig> FeatureRegistration<FeatureConfig> {
    /// Creates a new `FeatureRegistration` from the given registration data
    ///
    /// The `node_name` field will be filled in from the name of the `node` parameter.
    ///
    /// # Safety
    ///
    /// - The `arc_name` field must point to a valid nul-terminated C string that lives for the
    ///   lifetime of the registration.
    /// - The `runs_before` and `runs_after` fields must either be a valid null-terminated array
    ///   of valid pointers to nul-terminated C string that live for the lifetime of the
    ///   registration, or null.
    /// - Other pointers may be left null.
    /// - [`Self::register`] must be called before calling other methods.
    pub const unsafe fn new<N: Node<FeatureData = FeatureConfig>, const N_NEXT_NODES: usize>(
        mut feat: vnet_feature_registration_t,
        node: &'static NodeRegistration<N, N_NEXT_NODES>,
    ) -> Self {
        feat.node_name = node.name_ptr().cast_mut();
        Self {
            feat: UnsafeCell::new(feat),
            _marker: PhantomData,
        }
    }

    /// Registers the feature with VPP
    ///
    /// # Safety
    ///
    /// - Must be called only once for this node registration.
    /// - Must be called from a constructor function that is invoked before VPP initialises.
    pub unsafe fn register(&'static self) {
        // SAFETY: it is safe for this method to take a non-mutable self and mutates the
        // vnet_feature_registration as it is done through an UnsafeCell and method
        // preconditions mean this is done at startup when there are no outstanding references,
        // including from other threads. The updating of the feature_main feature linked list is
        // in accordance with vpp expectations.
        unsafe {
            let fm = std::ptr::addr_of_mut!(feature_main);
            let r = self.feat.get();
            (*r).next = (*fm).next_feature;
            (*fm).next_feature = r;
        }
    }

    /// Unregisters the node with VPP
    ///
    /// # Safety
    ///
    /// - Must be called from a destructor function.
    /// - Must be called only once for this node registration.
    /// - The node must have been previously registered with VPP using [`Self::register`].
    pub unsafe fn unregister(&self) {
        // SAFETY: it is safe for this method to take a non-mutable self and mutates the
        // vnet_feature_registration as it is done through an UnsafeCell and method
        // preconditions mean this is done during a destructor when there are no outstanding
        // references, including from other threads. The updating of the feature_main feature
        // linked list is in accordance with vpp expectations.
        unsafe {
            let fm = std::ptr::addr_of_mut!(feature_main);
            let r = self.feat.get();
            vlib_helper_remove_feature_from_registrations(fm, r);
        }
    }

    /// Enables the feature on the given software interface index
    ///
    /// If `feature_config` is provided, it can be obtained by feature node when processing
    /// packets.
    ///
    /// If the feature arc doesn't operate on a per-interface basis, `sw_if_index` can be
    /// set to `SwIfIndex::new(0)`.
    pub fn enable(
        &self,
        _vm: &BarrierHeldMainRef,
        sw_if_index: SwIfIndex,
        feature_config: FeatureConfig,
    ) -> Result<(), VnetError> {
        // SAFETY: arc_name and node_name pointers are valid nul-terminated strings, the
        // function validates sw_if_index and returns an error if not valid, and non-concurrent
        // access is ensured by the fact that the method takes a &BarrierHeldMainRef, which can
        // only be obtained on the main thread and with no packets flowing (and potentially
        // using the state being modified).
        let ret = unsafe {
            let feat = self.feat.get().cast_const();
            vnet_feature_enable_disable(
                (*feat).arc_name,
                (*feat).node_name,
                sw_if_index.into(),
                1,
                std::ptr::addr_of!(feature_config).cast_mut().cast(),
                std::mem::size_of::<FeatureConfig>() as u32,
            )
        };
        if ret == 0 {
            Ok(())
        } else {
            Err(ret.into())
        }
    }

    /// Disables the feature on the given software interface index
    ///
    /// If the feature arc doesn't operate on a per-interface basis, `sw_if_index` can be
    /// set to `SwIfIndex::new(0)`.
    pub fn disable(
        &self,
        _vm: &BarrierHeldMainRef,
        sw_if_index: SwIfIndex,
    ) -> Result<(), VnetError> {
        // SAFETY: arc_name and node_name pointers are valid nul-terminated strings, the
        // function validates sw_if_index and returns an error if not valid, and non-concurrent
        // access is ensured by the fact that the method takes a &BarrierHeldMainRef, which can
        // only be obtained on the main thread and with no packets flowing (and potentially
        // using the state being modified).
        let ret = unsafe {
            let feat = self.feat.get().cast_const();
            vnet_feature_enable_disable(
                (*feat).arc_name,
                (*feat).node_name,
                sw_if_index.into(),
                0,
                std::ptr::null_mut(),
                0,
            )
        };
        if ret == 0 {
            Ok(())
        } else {
            Err(ret.into())
        }
    }
}

// SAFETY: all safe methods that mutate state should take a &VlibMainRef which can't be created by safe code or sent between thread, and which guarantees that we are running on either the main or worker threads. Then methods either use per-thread data, or restrict themselves to being able to run on a single thread (typically the main thread).
unsafe impl<T> Sync for FeatureRegistration<T> {}
