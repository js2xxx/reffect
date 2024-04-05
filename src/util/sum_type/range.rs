use super::repr::*;
use crate::util::tag::{Tag, UInt, UTerm};

pub trait Count {
    type Count: Tag;
}

impl Count for () {
    type Count = UTerm;
}

impl<Head, Tail> Count for (Head, Tail)
where
    Tail: Count,
{
    type Count = UInt<Tail::Count>;
}

pub trait Concat<T> {
    type Output;
}

impl<T> Concat<T> for () {
    type Output = (T, ());
}

impl<Head, Tail, T> Concat<T> for (Head, Tail)
where
    Tail: Concat<T>,
{
    type Output = (Head, <Tail as Concat<T>>::Output);
}

pub trait ConcatList<TList> {
    type Output;
}

impl<T> ConcatList<()> for T {
    type Output = T;
}

impl<Head, Tail, T> ConcatList<(Head, Tail)> for T
where
    T: Concat<Head>,
    T::Output: ConcatList<Tail>,
{
    type Output = <T::Output as ConcatList<Tail>>::Output;
}


pub trait SplitList<TList: SumList, UList>: SumList {
    type Remainder: SumList;

    fn broaden_tag(tag: u8) -> u8;

    fn narrow_tag(tag: u8) -> Result<u8, u8>;
}

impl<T: SumList> SplitList<(), ()> for T {
    type Remainder = Self;

    fn broaden_tag(tag: u8) -> u8 {
        unreachable!("mapping tag {tag} from an empty set")
    }

    fn narrow_tag(tag: u8) -> Result<u8, u8> {
        Err(tag)
    }
}

impl<SubHead, SubTail, SuperHead, SuperTail, HeadTag: Tag, TailTag>
    SplitList<(SubHead, SubTail), (HeadTag, TailTag)> for (SuperHead, SuperTail)
where
    SubTail: SumList,
    SuperTail: SumList,

    (SuperHead, SuperTail): Split<SubHead, HeadTag>,

    <(SuperHead, SuperTail) as Split<SubHead, HeadTag>>::Remainder: SplitList<SubTail, TailTag>,
{
    type Remainder =
        <<(SuperHead, SuperTail) as Split<SubHead, HeadTag>>::Remainder as SplitList<
            SubTail,
            TailTag,
        >>::Remainder;

    fn broaden_tag(tag: u8) -> u8 {
        match <(SubHead, SubTail) as Split<SubHead, UTerm>>::try_unwrap(tag) {
            Ok(()) => HeadTag::VALUE,
            Err(remainder) => {
                let ret =
                    <<(SuperHead, SuperTail) as Split<SubHead, HeadTag>>::Remainder>::broaden_tag(
                        remainder,
                    );
                <(SuperHead, SuperTail) as Split<SubHead, HeadTag>>::from_remainder(ret)
            }
        }
    }

    fn narrow_tag(tag: u8) -> Result<u8, u8> {
        Ok(match Self::try_unwrap(tag) {
            Ok(()) => 0,
            Err(remainder) => {
                let ret =
                    <<(SuperHead, SuperTail) as Split<SubHead, HeadTag>>::Remainder>::narrow_tag(
                        remainder,
                    )?;
                <(SubHead, SubTail) as Split<SubHead, UTerm>>::from_remainder(ret)
            }
        })
    }
}

pub trait ContainsList<TList: SumList, UList, RemUList>:
    SplitList<TList, UList> + SplitList<<Self as SplitList<TList, UList>>::Remainder, RemUList>
{
}

impl<S, T, U, R> ContainsList<T, U, R> for S
where
    S: SplitList<T, U> + SplitList<<Self as SplitList<T, U>>::Remainder, R>,
    T: SumList,
{
}
