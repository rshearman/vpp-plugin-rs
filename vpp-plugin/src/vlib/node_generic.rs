//! Generic node implementations
//!
//! This module contains generic implementations of VPP nodes following set patterns that can be
//! reused across different plugins.

use std::mem::MaybeUninit;

use arrayvec::ArrayVec;

use crate::{
    vlib::{
        self, BufferIndex, MainRef,
        buffer::BufferRef,
        node::{FRAME_SIZE, FrameRef, NextNodes, Node, NodeRuntimeRef},
    },
    vppinfra::{likely, unlikely},
};

/// Next node to send a buffer to from a generic node implementation
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum FeatureNextNode<NextNode> {
    /// A specific next node defined by the generic node implementation
    DefinedNode(NextNode),
    /// The next feature in the feature arc
    NextFeature,
}

impl<NextNode> From<NextNode> for FeatureNextNode<NextNode> {
    fn from(value: NextNode) -> Self {
        Self::DefinedNode(value)
    }
}

/// Trait for generic node implementations processing one buffer at a time in a feature arc
pub trait GenericFeatureNodeX1<N: Node> {
    /// Processing a buffer and determining the next node to send it to
    ///
    /// # Safety
    ///
    /// The safety preconditions vary depending on the specific implementation.
    unsafe fn map_buffer_to_next(
        &self,
        vm: &MainRef,
        node: &mut NodeRuntimeRef<N>,
        b0: &mut BufferRef<N::FeatureData>,
    ) -> FeatureNextNode<N::NextNodes>;
}

/// Generic implementation of a VPP node processing one buffer at a time in a feature arc
///
/// # Safety
///
/// - The preconditions of the [`GenericFeatureNodeX1::map_buffer_to_next`] method must be upheld.
/// - Nodes with this node as a next node must send valid buffer indices in the Vector data.
/// - This mode must be invoked as part of a feature arc.
/// - All of the next nodes of this node must have a `Vector` type of `BufferIndex`, `Scalar` of
///   `()` and `Aux` of `()` (or their C equivalents).
#[inline(always)]
pub unsafe fn generic_feature_node_x1<GenericNode, N, FeatureData>(
    vm: &MainRef,
    node: &mut NodeRuntimeRef<N>,
    frame: &mut FrameRef<N>,
    generic_node_impl: GenericNode,
) -> u16
where
    N: Node<Vector = BufferIndex, Scalar = (), Aux = (), FeatureData = FeatureData>,
    GenericNode: GenericFeatureNodeX1<N>,
    FeatureData: Copy,
{
    // SAFETY: The safety requirements are documented in the function's safety comment.
    unsafe {
        let mut nexts: [MaybeUninit<u16>; FRAME_SIZE] = [MaybeUninit::uninit(); FRAME_SIZE];
        let mut b = ArrayVec::new();

        let from = frame.get_buffers::<FRAME_SIZE>(vm, &mut b);

        for (i, b0) in b.iter_mut().enumerate() {
            let next = generic_node_impl.map_buffer_to_next(vm, node, b0);
            let next = match next {
                FeatureNextNode::NextFeature => b0.vnet_feature_next().0 as u16,
                FeatureNextNode::DefinedNode(next) => next.into_u16(),
            };
            nexts.get_unchecked_mut(i).write(next);
        }

        // SAFETY: since every buffer yielded a next node and the number of elements of nexts is the
        // same as from, then every element is initialised. In addition, since we got all of the
        // buffer indices from `frame.get_buffers()` then they must all be valid. All the next nodes
        // expect to receive buffer indices and no other vector, aux or scalar data.
        let initialized_nexts = nexts.get_unchecked(..b.len()).assume_init_ref();
        vm.buffer_enqueue_to_next(node, from, initialized_nexts);

        frame.vector().len() as u16
    }
}

/// Trait for generic node implementations processing one buffer at a time in a feature arc
pub trait GenericFeatureNodeX4<N: Node>: GenericFeatureNodeX1<N> {
    /// Performing prefetching for a given buffer
    fn prefetch_buffer_x4(
        &self,
        _vm: &MainRef,
        _node: &mut NodeRuntimeRef<N>,
        b: &mut [&mut BufferRef<N::FeatureData>; 4],
    ) {
        // By default we assume that at the very least the buffer headers will be read from, but
        // plugins should generally override this for their specifics
        b[0].prefetch_header_load();
        b[1].prefetch_header_load();
        b[2].prefetch_header_load();
        b[3].prefetch_header_load();
    }

    /// Process four buffers and determining the next nodes to send them to
    ///
    /// # Safety
    ///
    /// The safety preconditions vary depending on the specific implementation.
    unsafe fn map_buffer_to_next_x4(
        &self,
        vm: &MainRef,
        node: &mut NodeRuntimeRef<N>,
        b: &mut [&mut BufferRef<N::FeatureData>; 4],
    ) -> [FeatureNextNode<N::NextNodes>; 4];

    /// Trace a buffer
    ///
    /// This is optional and can be empty if tracing is implemented in
    /// [`GenericFeatureNodeX4::map_buffer_to_next_x4`] and
    /// [`GenericFeatureNodeX1::map_buffer_to_next`] instead.
    ///
    /// # Safety
    ///
    /// The safety preconditions vary depending on the specific implementation.
    unsafe fn trace_buffer(
        &self,
        _vm: &MainRef,
        _node: &mut NodeRuntimeRef<N>,
        _b0: &mut BufferRef<N::FeatureData>,
    ) {
    }
}

/// Generic implementation of a VPP node processing four buffers at a time in a feature arc
///
/// # Safety
///
/// - The preconditions of the [`GenericFeatureNodeX1::map_buffer_to_next`] &
///   [`GenericFeatureNodeX4::map_buffer_to_next_x4`] methods must be upheld.
/// - Nodes with this node as a next node must send valid buffer indices in the Vector data.
/// - This mode must be invoked as part of a feature arc.
/// - All of the next nodes of this node must have a `Vector` type of `BufferIndex`, `Scalar` of
///   `()` and `Aux` of `()` (or their C equivalents).
#[inline(always)]
pub unsafe fn generic_feature_node_x4<GenericNode, N, FeatureData>(
    vm: &MainRef,
    node: &mut NodeRuntimeRef<N>,
    frame: &mut FrameRef<N>,
    generic_node_impl: GenericNode,
) -> u16
where
    N: Node<Vector = BufferIndex, Scalar = (), Aux = (), FeatureData = FeatureData>,
    GenericNode: GenericFeatureNodeX4<N>,
    FeatureData: Copy,
{
    // SAFETY: The safety requirements are documented in the function's safety comment.
    unsafe {
        let mut nexts: [MaybeUninit<u16>; FRAME_SIZE] = [MaybeUninit::uninit(); FRAME_SIZE];
        let mut b = ArrayVec::new();

        let from = frame.get_buffers::<FRAME_SIZE>(vm, &mut b);
        let len = b.len();

        for stride in 0..len / 4 {
            let i = stride * 4;

            if i + 8 <= len {
                let stride_b = b.get_unchecked_mut(i + 4..i + 8);
                // Convert into array for type safety for trait method
                let stride_b = stride_b.as_mut_array::<4>().unwrap_unchecked();

                generic_node_impl.prefetch_buffer_x4(vm, node, stride_b);
            }

            let stride_b = b.get_unchecked_mut(i..i + 4);
            let stride_nexts = nexts.get_unchecked_mut(i..i + 4);
            // Convert into array for type safety for trait method
            let stride_b = stride_b.as_mut_array::<4>().unwrap_unchecked();
            let stride_nexts = stride_nexts.as_mut_array::<4>().unwrap_unchecked();

            // Optimise for the common case where all four buffers sit at the same point of the
            // same feature config string (same arc and config index, i.e. typically the same rx
            // interface) and won't be dropped: one config lookup serves all four, and allows for
            // use of vectorised store of nexts[i..i + 3]. Equal arc indices alone are not
            // enough — buffers from interfaces with different feature sets share an arc but
            // not a config string — and every buffer's config index must be advanced, or the
            // next feature on the arc re-reads this node's entry.
            let c0 = stride_b[0].current_config_index();
            // From VPP 26.06 every arc's config strings share one heap, so the index alone
            // identifies the point in the string.
            #[cfg(not(vpp_shared_feature_config_heap))]
            let same_arc = stride_b[0].vnet_buffer().feature_arc_index()
                == stride_b[1].vnet_buffer().feature_arc_index()
                && stride_b[0].vnet_buffer().feature_arc_index()
                    == stride_b[2].vnet_buffer().feature_arc_index()
                && stride_b[0].vnet_buffer().feature_arc_index()
                    == stride_b[3].vnet_buffer().feature_arc_index();
            #[cfg(vpp_shared_feature_config_heap)]
            let same_arc = true;
            if likely(
                same_arc
                    && c0 == stride_b[1].current_config_index()
                    && c0 == stride_b[2].current_config_index()
                    && c0 == stride_b[3].current_config_index(),
            ) {
                let feature_next = stride_b[0].vnet_feature_next().0 as u16;
                let advanced = stride_b[0].current_config_index();
                stride_b[1].set_current_config_index(advanced);
                stride_b[2].set_current_config_index(advanced);
                stride_b[3].set_current_config_index(advanced);
                stride_nexts[0].write(feature_next);
                stride_nexts[1].write(feature_next);
                stride_nexts[2].write(feature_next);
                stride_nexts[3].write(feature_next);
            } else {
                stride_nexts[0].write(stride_b[0].vnet_feature_next().0 as u16);
                stride_nexts[1].write(stride_b[1].vnet_feature_next().0 as u16);
                stride_nexts[2].write(stride_b[2].vnet_feature_next().0 as u16);
                stride_nexts[3].write(stride_b[3].vnet_feature_next().0 as u16);
            };

            let feat_nexts = generic_node_impl.map_buffer_to_next_x4(vm, node, stride_b);

            for (next_i, next) in feat_nexts.into_iter().enumerate() {
                match next {
                    FeatureNextNode::NextFeature => { /* already set */ }
                    FeatureNextNode::DefinedNode(next) => {
                        stride_nexts[next_i].write(next.into_u16());
                    }
                };
            }
        }

        for i in (len / 4) * 4..len {
            let b0 = b.get_unchecked_mut(i);

            // Optimise for the case where the packet won't be processed by the feature
            let mut next_val = b0.vnet_feature_next().0 as u16;

            let next = generic_node_impl.map_buffer_to_next(vm, node, b0);
            match next {
                FeatureNextNode::NextFeature => { /* already set */ }
                FeatureNextNode::DefinedNode(next) => next_val = next.into_u16(),
            };
            nexts.get_unchecked_mut(i).write(next_val);
        }

        // Although it might seem more natural to check the frame flags, existing practice amongst
        // C plugins is to check the node runtime flags instead, so that is what is followed here.
        if unlikely(node.flags().contains(vlib::node::NodeFlags::TRACE)) {
            for b0 in &mut b {
                if b0.flags().contains(vlib::BufferFlags::IS_TRACED) {
                    generic_node_impl.trace_buffer(vm, node, b0);
                }
            }
        }

        // SAFETY: since every buffer yielded a next node and the number of elements of nexts is the
        // same as from, then every element is initialised. In addition, since we got all of the
        // buffer indices from `frame.get_buffers()` then they must all be valid. All the next nodes
        // expect to receive buffer indices and no other vector, aux or scalar data.
        let initialized_nexts = nexts.get_unchecked(..b.len()).assume_init_ref();
        vm.buffer_enqueue_to_next(node, from, initialized_nexts);

        frame.vector().len() as u16
    }
}
