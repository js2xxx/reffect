use core::{
    marker::PhantomData,
    ops::{Coroutine, CoroutineState},
    pin::Pin,
    task::Poll,
};

use futures_core::stream::Stream;
use pin_project::pin_project;

use crate::{
    Effect, EffectExt, Effectful,
    adapter::Begin,
    effect::{IntoCoroutine, ResumeTy},
    util::Sum,
};

#[derive(Debug, Clone, Copy)]
pub struct AsyncIter<T>(pub Poll<T>);

impl<T> Effect for AsyncIter<T> {
    type Resume = crate::future::ResumeTy;
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pin_project]
pub struct StreamCoro<S: Stream>(#[pin] S);

impl<S: Stream> StreamCoro<S> {
    fn with<T>(self: Pin<&mut Self>, f: impl FnOnce(Pin<&mut S>) -> T) -> T {
        f(self.project().0)
    }
}

type EffectStream<T> = crate::Sum![AsyncIter<T>];
type ResumeStream<T> = crate::Sum![Begin, ResumeTy<AsyncIter<T>>];

impl<S, T> Coroutine<ResumeStream<T>> for StreamCoro<S>
where
    S: Stream<Item = T>,
{
    type Yield = EffectStream<T>;

    type Return = ();

    fn resume(
        self: Pin<&mut Self>,
        arg: ResumeStream<T>,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        let mut resume_ty = match arg.try_unwrap::<ResumeTy<AsyncIter<T>>, _>() {
            Ok(ty) => ty,
            Err(_) => return CoroutineState::Yielded(Sum::new(AsyncIter(Poll::Pending))),
        };
        // SAFETY: The reference is guaranteed to be valid for the span of
        // this polling lifetime.
        match self.with(|s| s.poll_next(unsafe { resume_ty.0.as_mut() })) {
            Poll::Ready(Some(item)) => {
                CoroutineState::Yielded(Sum::new(AsyncIter(Poll::Ready(item))))
            }
            Poll::Ready(None) => CoroutineState::Complete(()),
            Poll::Pending => CoroutineState::Yielded(Sum::new(AsyncIter(Poll::Pending))),
        }
    }
}

impl<S, T> Stream for StreamCoro<S>
where
    S: Stream<Item = T>,
{
    type Item = T;

    fn poll_next(
        self: Pin<&mut Self>,
        cx: &mut core::task::Context<'_>,
    ) -> Poll<Option<Self::Item>> {
        self.with(|s| s.poll_next(cx))
    }
}

pub enum StreamMarker {}
impl<S, T> IntoCoroutine<StreamMarker, ResumeStream<T>> for S
where
    S: Stream<Item = T>,
{
    type Yield = EffectStream<T>;
    type Return = ();
    type IntoCoroutine = StreamCoro<S>;

    fn into_coroutine(self) -> Self::IntoCoroutine {
        StreamCoro(self)
    }
}

pub fn from_effectful<Coro, T>(coro: Coro) -> impl Stream<Item = T>
where
    Coro: Effectful<(AsyncIter<T>, ()), Return = ()>,
{
    #[pin_project]
    struct CoroStream<Coro, T>(#[pin] Coro, PhantomData<T>);

    impl<Coro, T> Stream for CoroStream<Coro, T>
    where
        Coro: Effectful<(AsyncIter<T>, ()), Return = ()>,
    {
        type Item = T;

        fn poll_next(
            self: Pin<&mut Self>,
            cx: &mut core::task::Context<'_>,
        ) -> Poll<Option<Self::Item>> {
            let mut coro = self.project().0;
            // SAFETY: The reference is guaranteed to be valid for the span of
            // this polling.
            let state = Sum::new(AsyncIter::tag(unsafe { crate::future::ResumeTy::new(cx) }));
            match Pin::new(&mut coro).resume(state) {
                CoroutineState::Yielded(item) => match item.into_inner() {
                    AsyncIter(Poll::Ready(ret)) => Poll::Ready(Some(ret)),
                    AsyncIter(Poll::Pending) => Poll::Pending,
                },
                CoroutineState::Complete(()) => Poll::Ready(None),
            }
        }
    }

    CoroStream(coro, PhantomData)
}
