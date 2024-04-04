use tuple_list::{Tuple, TupleList};

use super::repr::*;
use crate::util::tag::U0;

pub trait TupleRange<
    TList: TupleList = Self,
    UList = <<<Self as TupleList>::Tuple as TupleSum>::Repr as Repr>::Tags,
>: TupleList where
    Self::Tuple: TupleSum,
    TList::Tuple: TupleSum,
{
    type Remainder: Repr;

    fn narrow(
        this: <Self::Tuple as TupleSum>::Repr,
    ) -> Result<<TList::Tuple as TupleSum>::Repr, Self::Remainder>;

    fn broaden(this: <TList::Tuple as TupleSum>::Repr) -> <Self::Tuple as TupleSum>::Repr;
}

pub trait TupleMap<TList: TupleList, TList2: TupleList, UList>: TupleRange<TList, UList>
where
    Self::Tuple: TupleSum,
    TList::Tuple: TupleSum,
    TList2::Tuple: TupleSum,
{
    type Substitute: Repr;

    fn map(
        this: <Self::Tuple as TupleSum>::Repr,
        f: impl FnOnce(<TList::Tuple as TupleSum>::Repr) -> <TList2::Tuple as TupleSum>::Repr,
    ) -> Self::Substitute;
}

impl<T: TupleList> TupleRange<(), ()> for T
where
    Self::Tuple: TupleSum,
{
    type Remainder = <Self::Tuple as TupleSum>::Repr;

    fn narrow(err: <Self::Tuple as TupleSum>::Repr) -> Result<E0, <Self::Tuple as TupleSum>::Repr> {
        Err(err)
    }

    fn broaden(_: E0) -> <Self::Tuple as TupleSum>::Repr {
        unreachable!()
    }
}

impl<T: TupleList, TList2: TupleList> TupleMap<(), TList2, ()> for T
where
    Self::Tuple: TupleSum,
    TList2::Tuple: TupleSum,
{
    type Substitute = <Self::Tuple as TupleSum>::Repr;
    fn map(
        this: <Self::Tuple as TupleSum>::Repr,
        _: impl FnOnce(E0) -> <TList2::Tuple as TupleSum>::Repr,
    ) -> <Self::Tuple as TupleSum>::Repr {
        this
    }
}

impl<SubHead, SubTail, SuperHead, SuperTail, HeadTag, TailTag>
    TupleRange<(SubHead, SubTail), (HeadTag, TailTag)> for (SuperHead, SuperTail)
where
    SubTail: TupleList,
    SuperTail: TupleList,
    SubTail::Tuple: TupleSum,
    SuperTail::Tuple: TupleSum,

    (SubHead, SubTail): TupleList,
    (SuperHead, SuperTail): TupleList,
    <(SubHead, SubTail) as TupleList>::Tuple: TupleSum,
    <(SuperHead, SuperTail) as TupleList>::Tuple: TupleSum,

    <<(SubHead, SubTail) as TupleList>::Tuple as TupleSum>::Repr:
        ReprMatch<SubHead, U0, Remainder = <<SubTail as TupleList>::Tuple as TupleSum>::Repr>,
    <<(SuperHead, SuperTail) as TupleList>::Tuple as TupleSum>::Repr: ReprMatch<SubHead, HeadTag>,
    <<<<<(SuperHead, SuperTail) as TupleList>::Tuple as TupleSum>::Repr as ReprMatch<
        SubHead,
        HeadTag,
    >>::Remainder as Repr>::Tuple as Tuple>::TupleList: TupleRange<SubTail, TailTag>,
{
    type Remainder = <<<<<<(SuperHead, SuperTail) as TupleList>
        ::Tuple as TupleSum>::Repr as ReprMatch<SubHead, HeadTag>>
        ::Remainder as Repr>::Tuple as Tuple>
        ::TupleList as TupleRange<SubTail, TailTag>>
        ::Remainder;

    fn narrow(
        this: <<(SuperHead, SuperTail) as TupleList>::Tuple as TupleSum>::Repr,
    ) -> Result<<<(SubHead, SubTail) as TupleList>::Tuple as TupleSum>::Repr, Self::Remainder> {
        match this.try_unwrap() {
            Ok(value) => Ok(ReprMatch::new(value)),
            Err(err) => Ok(ReprMatch::from_remainder(
                <<<<<<(SuperHead, SuperTail) as TupleList>::Tuple as TupleSum>::Repr as ReprMatch<
                    SubHead,
                    HeadTag,
                >>::Remainder as Repr>::Tuple as Tuple>::TupleList as TupleRange<
                    SubTail,
                    TailTag,
                >>::narrow(err)?,
            )),
        }
    }

    fn broaden(
        this: <<(SubHead, SubTail) as TupleList>::Tuple as TupleSum>::Repr,
    ) -> <<(SuperHead, SuperTail) as TupleList>::Tuple as TupleSum>::Repr {
        match this.try_unwrap() {
            Ok(value) => ReprMatch::new(value),
            Err(err) => ReprMatch::from_remainder(
                <<<<<<(SuperHead, SuperTail) as TupleList>::Tuple as TupleSum>::Repr as ReprMatch<
                    SubHead,
                    HeadTag,
                >>::Remainder as Repr>::Tuple as Tuple>::TupleList as TupleRange<
                    SubTail,
                    TailTag,
                >>::broaden(err),
            ),
        }
    }
}

pub trait TupleBirange<TList: TupleList, UList, RemUList>:
    TupleList
    + TupleRange<TList, UList>
    + TupleRange<
        <<<Self as TupleRange<
            TList,
            UList,
        >>::Remainder as super::repr::Repr>::Tuple as Tuple>::TupleList,
        RemUList,
    >
where
    Self::Tuple: TupleSum,
    TList::Tuple: TupleSum,
{
}

impl<S, T, U, R> TupleBirange<T, U, R> for S
where
    S: TupleList
        + TupleRange<T, U>
        + TupleRange<
            <<<<<Self as TupleList>::Tuple as Tuple>::TupleList as TupleRange<
                <T::Tuple as Tuple>::TupleList,
                U,
            >>::Remainder as super::repr::Repr>::Tuple as Tuple>::TupleList,
            R,
        >,
    S::Tuple: TupleSum,
    T: TupleList,
    T::Tuple: TupleSum,
{
}
