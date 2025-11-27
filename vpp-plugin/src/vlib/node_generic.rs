//! Generic node implementations
//!
//! This module contains generic implementations of VPP nodes following set patterns that can be
//! reused across different plugins.

use core::slice;
use std::mem::MaybeUninit;

use arrayvec::ArrayVec;

use crate::{
    bindings::VLIB_NODE_FLAG_TRACE,
    vlib::{
        self,
        buffer::BufferRef,
        node::{FrameRef, NextNodes, Node, NodeRuntimeRef, FRAME_SIZE},
        BufferIndex, MainRef,
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
    vm.buffer_enqueue_to_next(
        node,
        from,
        std::mem::transmute::<&[MaybeUninit<u16>], &[u16]>(slice::from_raw_parts(
            nexts.as_ptr(),
            b.len(),
        )),
    );

    frame.vector().len() as u16
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
    let mut nexts: [MaybeUninit<u16>; FRAME_SIZE] = [MaybeUninit::uninit(); FRAME_SIZE];
    let mut b = ArrayVec::new();

    let from = frame.get_buffers::<FRAME_SIZE>(vm, &mut b);
    let len = b.len();

    for stride in 0..len / 4 {
        let i = stride * 4;

        if stride * 4 + 4 < len {
            let stride_b = b.get_unchecked_mut(i + 4..i + 8);
            // Convert into array for type safety for trait method
            let stride_b: &mut [&mut BufferRef<_>; 4] = stride_b.try_into().unwrap_unchecked();

            generic_node_impl.prefetch_buffer_x4(vm, node, stride_b);
        }

        let stride_b = b.get_unchecked_mut(i..i + 4);
        // Convert into array for type safety for trait method
        let stride_b: &mut [&mut BufferRef<_>; 4] = stride_b.try_into().unwrap_unchecked();

        // Optimise for common case where feature arc indices are the same for
        // all packets and for the case where the packet won't be dropped. This
        // allows for use of vectorised store of nexts[i..i + 3].
        if likely(
            stride_b[0].vnet_buffer().feature_arc_index()
                == stride_b[1].vnet_buffer().feature_arc_index()
                && stride_b[0].vnet_buffer().feature_arc_index()
                    == stride_b[2].vnet_buffer().feature_arc_index()
                && stride_b[0].vnet_buffer().feature_arc_index()
                    == stride_b[3].vnet_buffer().feature_arc_index(),
        ) {
            let feature_next = stride_b[0].vnet_feature_next().0 as u16;
            nexts.get_unchecked_mut(i).write(feature_next);
            nexts.get_unchecked_mut(i + 1).write(feature_next);
            nexts.get_unchecked_mut(i + 2).write(feature_next);
            nexts.get_unchecked_mut(i + 3).write(feature_next);
        } else {
            nexts
                .get_unchecked_mut(i)
                .write(stride_b[0].vnet_feature_next().0 as u16);
            nexts
                .get_unchecked_mut(i + 1)
                .write(stride_b[1].vnet_feature_next().0 as u16);
            nexts
                .get_unchecked_mut(i + 2)
                .write(stride_b[2].vnet_feature_next().0 as u16);
            nexts
                .get_unchecked_mut(i + 3)
                .write(stride_b[3].vnet_feature_next().0 as u16);
        };

        let feat_nexts = generic_node_impl.map_buffer_to_next_x4(vm, node, stride_b);

        for (next_i, next) in feat_nexts.into_iter().enumerate() {
            match next {
                FeatureNextNode::NextFeature => { /* already set */ }
                FeatureNextNode::DefinedNode(next) => {
                    nexts.get_unchecked_mut(i + next_i).write(next.into_u16());
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

    if unlikely(((*node.as_ptr()).flags & VLIB_NODE_FLAG_TRACE as u16) != 0) {
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
    vm.buffer_enqueue_to_next(
        node,
        from,
        std::mem::transmute::<&[MaybeUninit<u16>], &[u16]>(slice::from_raw_parts(
            nexts.as_ptr(),
            b.len(),
        )),
    );

    frame.vector().len() as u16
}
