use core::{mem::ManuallyDrop, ptr};

use super::range::TupleCount;
use crate::util::tag::{Tag, UInt, UTerm};

#[doc(hidden)]
pub struct Nil;

#[doc(hidden)]
#[repr(C)]
#[derive(Clone, Copy)]
pub union Cons<T, U> {
    pub(super) data: ManuallyDrop<T>,
    pub(super) next: ManuallyDrop<U>,
}

pub trait TupleSum: TupleCount {
    type Repr;
    type Tags<U>;

    #[doc(hidden)]
    unsafe fn drop(this: &mut ManuallyDrop<Self::Repr>, tag: u8);
}

impl TupleSum for () {
    type Repr = Nil;
    type Tags<U> = ();

    unsafe fn drop(_: &mut ManuallyDrop<Nil>, _: u8) {}
}

impl<Head, Tail> TupleSum for (Head, Tail)
where
    Tail: TupleSum,
{
    type Repr = Cons<Head, Tail::Repr>;
    type Tags<U> = (U, Tail::Tags<U>);

    unsafe fn drop(this: &mut ManuallyDrop<Self::Repr>, tag: u8) {
        if tag == 0 {
            unsafe { ManuallyDrop::drop(&mut this.data) };
        } else {
            unsafe { Tail::drop(&mut this.next, tag - 1) }
        }
    }
}

pub trait TupleMatch<T, U: Tag>: TupleSum {
    #[doc(hidden)]
    fn from_data(data: T) -> Self::Repr;

    #[doc(hidden)]
    unsafe fn into_data_unchecked(this: Self::Repr) -> T;

    #[doc(hidden)]
    fn as_ptr(this: &Self::Repr) -> *const T;

    #[doc(hidden)]
    fn as_mut_ptr(this: &mut Self::Repr) -> *mut T;

    type Remainder: TupleSum;
    type Substitute<T2>: TupleMatch<T2, U>;

    #[doc(hidden)]
    fn from_remainder(tag: u8) -> u8;

    #[doc(hidden)]
    fn try_unwrap(tag: u8) -> Result<(), u8>;
}

impl<Head, Tail> TupleMatch<Head, UTerm> for (Head, Tail)
where
    Tail: TupleSum,
{
    fn from_data(data: Head) -> Self::Repr {
        Cons { data: ManuallyDrop::new(data) }
    }

    unsafe fn into_data_unchecked(this: Self::Repr) -> Head {
        unsafe { ManuallyDrop::into_inner(this.data) }
    }

    fn as_ptr(this: &Self::Repr) -> *const Head {
        let ptr = unsafe { ptr::addr_of!(this.data).cast::<Head>() };
        debug_assert_eq!(ptr.cast(), this as _);
        ptr
    }

    fn as_mut_ptr(this: &mut Self::Repr) -> *mut Head {
        let ptr = unsafe { ptr::addr_of_mut!(this.data).cast::<Head>() };
        debug_assert_eq!(ptr.cast(), this as _);
        ptr
    }

    type Remainder = Tail;
    type Substitute<T2> = (T2, Tail);

    fn from_remainder(tag: u8) -> u8 {
        tag + 1
    }

    fn try_unwrap(tag: u8) -> Result<(), u8> {
        match tag.checked_sub(1) {
            None => Ok(()),
            Some(tag) => Err(tag),
        }
    }
}

impl<Head, Tail, T, U: Tag> TupleMatch<T, UInt<U>> for (Head, Tail)
where
    Tail: TupleMatch<T, U>,
{
    fn from_data(data: T) -> Self::Repr {
        Cons {
            next: ManuallyDrop::new(Tail::from_data(data)),
        }
    }

    unsafe fn into_data_unchecked(this: Self::Repr) -> T {
        unsafe { Tail::into_data_unchecked(ManuallyDrop::into_inner(this.next)) }
    }

    fn as_ptr(this: &Self::Repr) -> *const T {
        let ptr = unsafe { Tail::as_ptr(&this.next) };
        debug_assert_eq!(ptr.cast(), this as _);
        ptr
    }

    fn as_mut_ptr(this: &mut Self::Repr) -> *mut T {
        let ptr = unsafe { Tail::as_mut_ptr(&mut this.next) };
        debug_assert_eq!(ptr.cast(), this as _);
        ptr
    }

    type Remainder = (Head, <Tail as TupleMatch<T, U>>::Remainder);
    type Substitute<T2> = (Head, Tail::Substitute<T2>);

    fn from_remainder(tag: u8) -> u8 {
        if tag < UInt::<U>::VALUE {
            tag
        } else {
            tag + 1
        }
    }

    fn try_unwrap(tag: u8) -> Result<(), u8> {
        let cur = UInt::<U>::VALUE;
        match tag.cmp(&cur) {
            core::cmp::Ordering::Equal => Ok(()),
            core::cmp::Ordering::Less => Err(tag),
            core::cmp::Ordering::Greater => Err(tag - 1),
        }
    }
}
