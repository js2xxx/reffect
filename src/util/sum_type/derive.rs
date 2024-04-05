use core::{fmt, hash::Hasher, mem::ManuallyDrop};

use super::repr::{Cons, Nil, TupleSum};

pub(super) trait TupleClone: TupleSum {
    unsafe fn clone(this: &Self::Repr, tag: u8) -> ManuallyDrop<Self::Repr>;
}

impl TupleClone for () {
    unsafe fn clone(_: &Self::Repr, _: u8) -> ManuallyDrop<Self::Repr>
    where
        Self: Clone,
    {
        ManuallyDrop::new(Nil)
    }
}

impl<Head, Tail> TupleClone for (Head, Tail)
where
    Head: Clone,
    Tail: TupleClone,
{
    unsafe fn clone(this: &Self::Repr, tag: u8) -> ManuallyDrop<Self::Repr> {
        ManuallyDrop::new(if tag == 0 {
            let data = unsafe { this.data.clone() };
            Cons { data }
        } else {
            let next = unsafe { Tail::clone(&this.next, tag - 1) };
            Cons { next }
        })
    }
}

pub(super) trait TupleDebug: TupleSum {
    unsafe fn debug(this: &Self::Repr, tag: u8) -> &dyn fmt::Debug;
}

impl TupleDebug for () {
    unsafe fn debug(_: &Self::Repr, _: u8) -> &dyn fmt::Debug {
        &()
    }
}

impl<Head, Tail> TupleDebug for (Head, Tail)
where
    Head: fmt::Debug,
    Tail: TupleDebug,
{
    unsafe fn debug(this: &Self::Repr, tag: u8) -> &dyn fmt::Debug {
        if tag == 0 {
            unsafe { &this.data }
        } else {
            unsafe { Tail::debug(&this.next, tag - 1) }
        }
    }
}

pub(super) trait TuplePartialEq: TupleSum {
    unsafe fn eq(this: &Self::Repr, other: &Self::Repr, tag: u8) -> bool;
}

impl TuplePartialEq for () {
    unsafe fn eq(_: &Self::Repr, _: &Self::Repr, _: u8) -> bool {
        true
    }
}

impl<Head, Tail> TuplePartialEq for (Head, Tail)
where
    Head: PartialEq,
    Tail: TuplePartialEq,
{
    unsafe fn eq(this: &Self::Repr, other: &Self::Repr, tag: u8) -> bool {
        if tag == 0 {
            unsafe { this.data == other.data }
        } else {
            unsafe { Tail::eq(&this.next, &other.next, tag - 1) }
        }
    }
}

pub(super) trait TuplePartialOrd: TupleSum + TuplePartialEq {
    unsafe fn partial_cmp(
        this: &Self::Repr,
        other: &Self::Repr,
        tag: u8,
    ) -> Option<core::cmp::Ordering>;
}

impl TuplePartialOrd for () {
    unsafe fn partial_cmp(_: &Self::Repr, _: &Self::Repr, _: u8) -> Option<core::cmp::Ordering> {
        Some(core::cmp::Ordering::Equal)
    }
}

impl<Head, Tail> TuplePartialOrd for (Head, Tail)
where
    Head: PartialOrd,
    Tail: TuplePartialOrd,
{
    unsafe fn partial_cmp(
        this: &Self::Repr,
        other: &Self::Repr,
        tag: u8,
    ) -> Option<core::cmp::Ordering> {
        if tag == 0 {
            unsafe { this.data.partial_cmp(&other.data) }
        } else {
            unsafe { Tail::partial_cmp(&this.next, &other.next, tag - 1) }
        }
    }
}

pub(super) trait TupleOrd: TupleSum + TuplePartialOrd {
    unsafe fn cmp(this: &Self::Repr, other: &Self::Repr, tag: u8) -> core::cmp::Ordering;
}

impl TupleOrd for () {
    unsafe fn cmp(_: &Self::Repr, _: &Self::Repr, _: u8) -> core::cmp::Ordering {
        core::cmp::Ordering::Equal
    }
}

impl<Head, Tail> TupleOrd for (Head, Tail)
where
    Head: Ord,
    Tail: TupleOrd,
{
    unsafe fn cmp(this: &Self::Repr, other: &Self::Repr, tag: u8) -> core::cmp::Ordering {
        if tag == 0 {
            unsafe { this.data.cmp(&other.data) }
        } else {
            unsafe { Tail::cmp(&this.next, &other.next, tag - 1) }
        }
    }
}

pub(super) trait TupleHash: TupleSum {
    unsafe fn hash<H: Hasher>(this: &Self::Repr, tag: u8, state: &mut H);
}

impl TupleHash for () {
    unsafe fn hash<H: Hasher>(_: &Self::Repr, _: u8, _: &mut H) {}
}

impl<Head, Tail> TupleHash for (Head, Tail)
where
    Head: core::hash::Hash,
    Tail: TupleHash,
{
    unsafe fn hash<H: Hasher>(this: &Self::Repr, tag: u8, state: &mut H) {
        if tag == 0 {
            unsafe { this.data.hash(state) }
        } else {
            unsafe { Tail::hash(&this.next, tag - 1, state) }
        }
    }
}
