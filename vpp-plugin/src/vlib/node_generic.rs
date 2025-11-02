//! Generic node implementations
//!
//! This module contains generic implementations of VPP nodes following set patterns that can be
//! reused across different plugins.

use std::mem::MaybeUninit;

use arrayvec::ArrayVec;

use crate::vlib::{
    buffer::BufferRef,
    node::{FrameRef, NextNodes, Node, NodeRuntimeRef, FRAME_SIZE},
    BufferIndex, MainRef,
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
    let b_len = b.len();

    for (i, b0) in b.iter_mut().enumerate() {
        let next = generic_node_impl.map_buffer_to_next(vm, node, b0);
        let next = match next {
            FeatureNextNode::DefinedNode(next) => next.into_u16(),
            FeatureNextNode::NextFeature => b0.vnet_feature_next().0 as u16,
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
        std::mem::transmute::<&[MaybeUninit<u16>], &[u16]>(&nexts[..b.len()]),
    );

    b_len as u16
}
