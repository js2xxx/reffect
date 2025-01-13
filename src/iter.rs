use core::{
    ops::{Coroutine, CoroutineState},
    pin::Pin,
};

use crate::{Effect, EffectExt, Effectful, adapter::Begin, effect::IntoCoroutine, util::Sum};

#[derive(Debug, Clone, Copy)]
pub struct Iter<T>(pub T);

impl<T> Effect for Iter<T> {
    type Resume = ResumeTy;
}

#[derive(Debug)]
pub struct ResumeTy;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IterCoro<I>(I);

impl<I: Iterator> Unpin for IterCoro<I> {}

type EffectIter<T> = Sum<(Iter<T>, ())>;
type ResumeIter<T> = crate::Sum![Begin, crate::effect::ResumeTy<Iter<T>>];

impl<I, T> Coroutine<ResumeIter<T>> for IterCoro<I>
where
    I: Iterator<Item = T>,
{
    type Yield = EffectIter<T>;

    type Return = ();

    fn resume(
        mut self: Pin<&mut Self>,
        _: ResumeIter<T>,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        match self.0.next() {
            Some(ret) => CoroutineState::Yielded(Sum::new(Iter(ret))),
            None => CoroutineState::Complete(()),
        }
    }
}

impl<I, T> Iterator for IterCoro<I>
where
    I: Iterator<Item = T>,
{
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.next()
    }
}

pub enum IteratorMarker {}

impl<I, T> IntoCoroutine<IteratorMarker, ResumeIter<T>> for I
where
    I: Iterator<Item = T>,
{
    type Yield = EffectIter<T>;

    type Return = ();

    type IntoCoroutine = IterCoro<I>;

    fn into_coroutine(self) -> Self::IntoCoroutine {
        IterCoro(self)
    }
}

pub fn from_effectful<T, Coro>(effectful: Coro) -> impl Iterator<Item = T>
where
    Coro: Effectful<(Iter<T>, ()), Return = ()> + Unpin,
{
    gen fn imp<T, Coro>(mut effectful: Coro) -> T
    where
        Coro: Effectful<(Iter<T>, ()), Return = ()> + Unpin,
    {
        while let CoroutineState::Yielded(n) =
            Pin::new(&mut effectful).resume(Sum::new(Iter::tag(ResumeTy)))
        {
            yield n.into_inner().0
        }
    }

    imp(effectful)
}

#[cfg(test)]
mod test {
    use super::from_effectful;
    use crate::effect::IntoCoroutine;

    #[test]
    fn test_iter() {
        let iter = 0..5;
        let coro = iter.clone().into_coroutine();

        let wrapped = from_effectful(coro);
        wrapped.zip(iter).for_each(|(a, b)| assert_eq!(a, b));
    }
}
