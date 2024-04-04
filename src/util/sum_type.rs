pub mod range;
pub mod repr;

use core::{
    convert::Infallible, fmt, marker::PhantomData, ops::{Deref, DerefMut}
};

pub use tuple_list::tuple_list_type as umap;
use tuple_list::Tuple;

pub type Repr<S> = <S as repr::TupleSum>::Repr;

#[macro_export]
macro_rules! Sum {
    [$($t:ty),* $(,)?] => {
        $crate::util::Sum::<($($t,)*)>
    };
}

pub struct Sum<S: repr::TupleSum>(Repr<S>);

impl<T> From<T> for Sum![T] {
    fn from(value: T) -> Self {
        Sum::new(value)
    }
}

impl<T> Deref for Sum![T] {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        match &self.0 {
            repr::E1::A(t) => t,
        }
    }
}

impl<T> DerefMut for Sum![T] {
    fn deref_mut(&mut self) -> &mut Self::Target {
        match &mut self.0 {
            repr::E1::A(t) => t,
        }
    }
}

impl<T> Sum![T] {
    pub fn into_inner(self) -> T {
        match self.0 {
            repr::E1::A(t) => t,
        }
    }
}

impl Sum![Infallible] {
    pub fn unreachable(self) -> ! {
        match self.0 {
            repr::E1::A(t) => match t {},
        }
    }
}

impl<S: repr::TupleSum> Sum<S> {
    pub fn new<T, U>(value: T) -> Self
    where
        Repr<S>: repr::ReprMatch<T, U>,
    {
        Sum(repr::ReprMatch::new(value))
    }

    pub fn type_marker(&self) -> PhantomData<S> {
        PhantomData
    }

    pub fn repr(&self) -> &Repr<S> {
        &self.0
    }

    pub fn into_repr(self) -> Repr<S> {
        self.0
    }

    pub fn from_repr(repr: Repr<S>) -> Self {
        Sum(repr)
    }

    pub fn get<T, U>(&self) -> Option<&T>
    where
        Repr<S>: repr::ReprMatch<T, U>,
    {
        repr::ReprMatch::get(&self.0)
    }

    pub fn get_mut<T, U>(&mut self) -> Option<&mut T>
    where
        Repr<S>: repr::ReprMatch<T, U>,
    {
        repr::ReprMatch::get_mut(&mut self.0)
    }
}

pub type Rem<S, T, U> = <<Repr<S> as repr::ReprMatch<T, U>>::Remainder as repr::Repr>::Tuple;
pub type Substitute<S, T, T2, U> =
    <<Repr<S> as repr::ReprMatch<T, U>>::Substitute<T2> as repr::Repr>::Tuple;

impl<S: repr::TupleSum> Sum<S> {
    pub fn try_unwrap<T, U>(self) -> Result<T, Sum<Rem<S, T, U>>>
    where
        Repr<S>: repr::ReprMatch<T, U>,
    {
        match repr::ReprMatch::try_unwrap(self.0) {
            Ok(value) => Ok(value),
            Err(err) => Err(Sum(err)),
        }
    }

    pub fn map<T, T2, U>(self, f: impl FnOnce(T) -> T2) -> Sum<Substitute<S, T, T2, U>>
    where
        Repr<S>: repr::ReprMatch<T, U>,
    {
        Sum(repr::ReprMatch::map(self.0, f))
    }
}

pub type NarrowRem<S, S2, UMap> = <<<S as Tuple>::TupleList as range::TupleRange<
    <S2 as Tuple>::TupleList,
    UMap,
>>::Remainder as repr::Repr>::Tuple;

impl<S: repr::TupleSum> Sum<S> {
    pub fn narrow<S2, UMap>(self) -> Result<Sum<S2>, Sum<NarrowRem<S, S2, UMap>>>
    where
        S2: repr::TupleSum,
        S::TupleList: range::TupleRange<S2::TupleList, UMap>,
    {
        <S::TupleList as range::TupleRange<S2::TupleList, UMap>>::narrow(self.0)
            .map(Sum)
            .map_err(Sum)
    }

    pub fn broaden<S2, UMap>(self) -> Sum<S2>
    where
        S2: repr::TupleSum,
        S2::TupleList: range::TupleRange<S::TupleList, UMap>,
    {
        Sum(<S2::TupleList as range::TupleRange<S::TupleList, UMap>>::broaden(self.0))
    }
}

impl<S: repr::TupleSum> fmt::Debug for Sum<S>
where
    S::Repr: fmt::Debug,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<S: repr::TupleSum> Clone for Sum<S>
where
    S::Repr: Clone,
{
    fn clone(&self) -> Self {
        Sum(self.0.clone())
    }
}

impl<S: repr::TupleSum> Copy for Sum<S> where S::Repr: Copy {}

impl<S: repr::TupleSum, T: repr::TupleSum> PartialEq<Sum<T>> for Sum<S>
where
    S::Repr: PartialEq<T::Repr>,
{
    fn eq(&self, other: &Sum<T>) -> bool {
        self.0 == other.0
    }
}

impl<S: repr::TupleSum> Eq for Sum<S> where S::Repr: Eq {}

impl<S: repr::TupleSum, T: repr::TupleSum> PartialOrd<Sum<T>> for Sum<S>
where
    S::Repr: PartialOrd<T::Repr>,
{
    fn partial_cmp(&self, other: &Sum<T>) -> Option<core::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

impl<S: repr::TupleSum> Ord for Sum<S>
where
    S::Repr: Ord,
{
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.0.cmp(&other.0)
    }
}

impl<S: repr::TupleSum> core::hash::Hash for Sum<S>
where
    S::Repr: core::hash::Hash,
{
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.0.hash(state)
    }
}

#[cfg(test)]
mod tests {
    use std::string::{String, ToString};

    use super::*;
    use crate::util::tag::*;

    #[test]
    fn basic() {
        type T0 = (u32,);
        type T1 = (u32, String);
        type T2 = (u32, String, u32);

        let sum: Sum<T0> = 12345.into();
        assert_eq!(sum.get(), Some(&12345));

        let mut sum: Sum<T1> = sum.broaden();
        assert_eq!(sum.get::<u32, _>(), Some(&12345));
        assert_eq!(sum.get::<_, U1>(), None);

        sum = Sum::new("Hello World!".to_string());
        assert_eq!(sum.get(), Some(&"Hello World!".to_string()));

        let sum: Sum<T2> = sum.broaden::<_, umap![U2, U1]>();
        assert_eq!(sum.get(), Some(&"Hello World!".to_string()));

        let sum: Sum<T1> = sum.narrow::<_, umap![U0, U0]>().unwrap();
        let sum: Sum<(String,)> = sum.narrow::<T0, _>().unwrap_err();
        assert_eq!(*sum, "Hello World!");
    }
}
