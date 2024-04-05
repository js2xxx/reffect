// use super::tag::{UInt, UTerm};

// pub trait Count {
//     type Value;
// }

// impl Count for () {
//     type Value = UTerm;
// }

// impl<Head, Tail> Count for (Head, Tail)
// where
//     Tail: Count,
// {
//     type Value = UInt<Tail::Value>;
// }

// pub type CountTL<T> = <T as Count>::Value;
// pub type CountT<T> = <<T as Tuple>::TupleList as Count>::Value;

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

pub type ConcatT<A, B> = <A as ConcatList<B>>::Output;

// pub trait Split<T, U> {
//     type Output;

//     type Prefix;
//     type Suffix;
// }

// impl<Head, Tail> Split<Head, UTerm> for (Head, Tail) {
//     type Output = Tail;

//     type Prefix = ();
//     type Suffix = Tail;
// }

// impl<Head, Tail, T, U> Split<T, UInt<U>> for (Head, Tail)
// where
//     Tail: Split<T, U>,
// {
//     type Output = (Head, <Tail as Split<T, U>>::Output);

//     type Prefix = (Head, <Tail as Split<T, U>>::Prefix);
//     type Suffix = <Tail as Split<T, U>>::Suffix;
// }

// pub trait SplitList<TList, UList> {
//     type Output;
// }

// impl<SList> SplitList<(), ()> for SList {
//     type Output = SList;
// }

// impl<SHead, STail, THead, TTail, UHead, UTail> SplitList<(THead, TTail),
// (UHead, UTail)>     for (SHead, STail)
// where
//     (SHead, STail): Split<THead, UHead>,
//     <(SHead, STail) as Split<THead, UHead>>::Output: SplitList<TTail, UTail>,
// {
//     type Output =
//         <<(SHead, STail) as Split<THead, UHead>>::Output as SplitList<TTail,
// UTail>>::Output; }

// pub type SplitTL<A, B, U> = <A as SplitList<B, U>>::Output;

// pub type SplitT<TupleA, TupleB, TupleU> = <<<TupleA as Tuple>::TupleList as
// SplitList<     <TupleB as Tuple>::TupleList,
//     <TupleU as Tuple>::TupleList,
// >>::Output as TupleList>::Tuple;

// pub trait Subseq<TList, UList> {
//     type Output;
// }

// impl<SList> Subseq<(), ()> for SList {
//     type Output = SList;
// }

// type SuffixSubseq<SList, THead, TTail, UHead, UTail> =
//     <<SList as Split<THead, UHead>>::Suffix as Subseq<TTail, UTail>>::Output;

// type ConcatSubseq<SList, THead, TTail, UHead, UTail> =
//     <<SList as Split<THead, UHead>>::Prefix as ConcatList<
//         SuffixSubseq<SList, THead, TTail, UHead, UTail>,
//     >>::Output;

// impl<SHead, STail, THead, TTail, UHead, UTail> Subseq<(THead, TTail), (UHead,
// UTail)>     for (SHead, STail)
// where
//     (SHead, STail): Split<THead, UHead>,
//     <(SHead, STail) as Split<THead, UHead>>::Suffix: Subseq<TTail, UTail>,
//     <(SHead, STail) as Split<THead, UHead>>::Prefix:
//         ConcatList<SuffixSubseq<(SHead, STail), THead, TTail, UHead, UTail>>,
// {
//     type Output = ConcatSubseq<(SHead, STail), THead, TTail, UHead, UTail>;
// }

// pub type SubseqT<TupleA, TupleB, TupleU> = <<<TupleA as Tuple>::TupleList as
// Subseq<     <TupleB as Tuple>::TupleList,
//     <TupleU as Tuple>::TupleList,
// >>::Output as TupleList>::Tuple;

// pub trait Intersect<TList, Count, SM, TM> {
//     type Output;
// }

// impl<SList, TList, SS, US, ST, UT, Output> Intersect<TList, CountTL<Output>,
// (SS, US), (ST, UT)>     for SList
// where
//     SList: Subseq<SS, US, Output = Output>,
//     TList: SplitList<ST, UT, Output = Output>,

//     Output: Count,
// {
//     type Output = Output;
// }

// pub trait Lcp<TList, U> {
//     type Output;
// }

// impl<SList> Lcp<(), UTerm> for SList {
//     type Output = ();
// }

// impl<THead, TTail> Lcp<(THead, TTail), UTerm> for () {
//     type Output = ();
// }

// impl<Head, STail, TTail> Lcp<(Head, TTail), UTerm> for (Head, STail) {
//     type Output = (Head, ());
// }

// impl<Head, STail, TTail, U> Lcp<(Head, TTail), UInt<U>> for (Head, STail)
// where
//     STail: Lcp<TTail, U>,
// {
//     type Output = (Head, STail::Output);
// }

// pub type LcpT<TupleA, TupleB, U> = <<<TupleA as Tuple>::TupleList as Lcp<
//     <TupleB as Tuple>::TupleList,
//     U,
// >>::Output as TupleList>::Tuple;

// pub trait SubCont<TList, U>: Count {
//     type Output;
// }

// impl<SList, TList> SubCont<TList, UTerm> for SList
// where
//     SList: Count,
//     SList: Lcp<TList, <SList as Count>::Value, Output = SList>,
// {
//     type Output = SList;
// }

// impl<SList, THead, TTail, U> SubCont<(THead, TTail), UInt<U>> for SList
// where
//     SList: SubCont<TTail, U>,
// {
//     type Output = <SList as SubCont<TTail, U>>::Output;
// }

// pub type SubContT<TupleA, TupleB, U> = <<<TupleA as Tuple>::TupleList as
// SubCont<     <TupleB as Tuple>::TupleList,
//     U,
// >>::Output as TupleList>::Tuple;

// #[cfg(test)]
// mod test {
//     use tuple_list::tuple_list_type as t;

//     use super::*;
//     use crate::util::tag::*;

//     #[test]
//     fn test_append() {
//         fn append<A: ConcatList<B, Output = C>, B, C>() {}

//         append::<(), (), ()>();
//         append::<t![u32, i32], t![bool], t![u32, i32, bool]>();
//     }

//     #[test]
//     fn test_split() {
//         fn split<A: SplitList<B, U, Output = C>, B, C, U>() {}

//         split::<(), (), (), _>();
//         split::<t![u32, i32], t![], t![u32, i32], _>();
//         split::<t![u32, i32, bool, char], t![i32, bool], t![u32, char], _>();
//     }

//     #[test]
//     fn test_intersect() {
//         fn intersect<A, B, C, Cnt, SM, TM>()
//         where
//             A: Intersect<B, Cnt, SM, TM, Output = C>,
//             C: Count<Value = Cnt>,
//         {
//         }

//         intersect::<(), (), (), _, _, _>();
//         intersect::<t![], t![u32, i32], t![], _, _, (_, t![UTerm, UTerm])>();
//         intersect::<t![u32], t![u32, i32], _, _, (t![], _), (t![i32], _)>();
//         intersect::<t![u32, u8], t![u32, i32], _, _, (t![u8], _), (t![i32],
// _)>();     }

//     #[test]
//     fn test_lcp() {
//         fn lcp<A: Lcp<B, U, Output = C>, B, C, U>() {}

//         lcp::<(), (), (), _>();
//         lcp::<t![u32, i32], t![], t![], _>();
//         lcp::<t![u32, i32], t![u32, char], t![u32], _>();
//         lcp::<t![u32, i32], t![u32, i32], t![u32, i32], U1>();
//     }

//     #[test]
//     fn test_subcont() {
//         fn test_subcont<A: SubCont<B, U, Output = C>, B, C, U>() {}

//         test_subcont::<(), (), (), _>();
//         test_subcont::<t![u32, i32], t![u32, char, u32, i32], t![u32, i32],
// _>();     }

//     #[test]
//     fn test_subseq() {
//         fn subseq<A: Subseq<B, U>, B, U>() {}

//         subseq::<(), (), _>();
//         subseq::<t![u32, char, i32], t![u32, i32], _>();
//     }
// }
