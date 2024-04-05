use super::repr::*;
use crate::util::tag::{Tag, UTerm};

pub trait TupleRange<TList: TupleSum, UList>: TupleSum {
    type Remainder: TupleSum;

    fn broaden_tag(tag: u8) -> u8;

    fn narrow_tag(tag: u8) -> Result<u8, u8>;
}

impl<T: TupleSum> TupleRange<(), ()> for T {
    type Remainder = Self;

    fn broaden_tag(tag: u8) -> u8 {
        unreachable!("mapping tag {tag} from an empty set")
    }

    fn narrow_tag(tag: u8) -> Result<u8, u8> {
        Err(tag)
    }
}

impl<SubHead, SubTail, SuperHead, SuperTail, HeadTag: Tag, TailTag>
    TupleRange<(SubHead, SubTail), (HeadTag, TailTag)> for (SuperHead, SuperTail)
where
    SubTail: TupleSum,
    SuperTail: TupleSum,

    (SuperHead, SuperTail): TupleMatch<SubHead, HeadTag>,

    <(SuperHead, SuperTail) as TupleMatch<SubHead, HeadTag>>::Remainder:
        TupleRange<SubTail, TailTag>,
{
    type Remainder =
        <<(SuperHead, SuperTail) as TupleMatch<SubHead, HeadTag>>::Remainder as TupleRange<
            SubTail,
            TailTag,
        >>::Remainder;

    fn broaden_tag(tag: u8) -> u8 {
        match <(SubHead, SubTail) as TupleMatch<SubHead, UTerm>>::try_unwrap(tag) {
            Ok(()) => HeadTag::VALUE,
            Err(remainder) => {
                let ret = <<(SuperHead, SuperTail) as TupleMatch<SubHead, HeadTag>>::Remainder>::broaden_tag(
                        remainder,
                    );
                <(SuperHead, SuperTail) as TupleMatch<SubHead, HeadTag>>::from_remainder(ret)
            }
        }
    }

    fn narrow_tag(tag: u8) -> Result<u8, u8> {
        Ok(match Self::try_unwrap(tag) {
            Ok(()) => 0,
            Err(remainder) => {
                let ret = <<(SuperHead, SuperTail) as TupleMatch<SubHead, HeadTag>>::Remainder>::narrow_tag(remainder)?;
                <(SubHead, SubTail) as TupleMatch<SubHead, UTerm>>::from_remainder(ret)
            }
        })
    }
}

pub trait TupleBirange<TList: TupleSum, UList, RemUList>:
    TupleRange<TList, UList> + TupleRange<<Self as TupleRange<TList, UList>>::Remainder, RemUList>
{
}

impl<S, T, U, R> TupleBirange<T, U, R> for S
where
    S: TupleRange<T, U> + TupleRange<<Self as TupleRange<T, U>>::Remainder, R>,
    T: TupleSum,
{
}
