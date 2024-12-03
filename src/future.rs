use core::{
    future::{Future, IntoFuture, poll_fn},
    ops::{Coroutine, CoroutineState},
    pin::Pin,
    ptr::NonNull,
    task::{Context, Poll},
};

use pin_project::pin_project;

use crate::{
    Effect, Effectful,
    adapter::Begin,
    effect::{EffectExt, IntoCoroutine},
    util::Sum,
};

#[derive(Debug, Clone, Copy)]
pub struct Async;

impl Effect for Async {
    type Resume = ResumeTy;
}

#[derive(Debug)]
pub struct ResumeTy(NonNull<Context<'static>>);

impl ResumeTy {
    /// # Safety
    ///
    /// The context must be valid for the span of upcoming `resume` calling.
    pub unsafe fn new(cx: &mut Context<'_>) -> Self {
        ResumeTy(NonNull::from(cx).cast())
    }
}

/// This type is `Send` because it doesn't depend on the fact that `&mut
/// Context<'_>` is not `Send` whether futures are `Send`.
unsafe impl Send for ResumeTy {}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[pin_project]
pub struct FutCoro<F: Future>(#[pin] F);

impl<F: Future> FutCoro<F> {
    fn with<T>(self: Pin<&mut Self>, f: impl FnOnce(Pin<&mut F>) -> T) -> T {
        f(self.project().0)
    }
}

type EffectAsync = Sum<(Async, ())>;
type ResumeAsync = crate::Sum![Begin, crate::effect::ResumeTy<Async>];

impl<F, T> Coroutine<ResumeAsync> for FutCoro<F>
where
    F: Future<Output = T>,
{
    type Yield = EffectAsync;

    type Return = T;

    fn resume(self: Pin<&mut Self>, arg: ResumeAsync) -> CoroutineState<EffectAsync, T> {
        self.with(|fut| {
            let mut resume_ty = match arg.try_unwrap::<crate::effect::ResumeTy<Async>, _>() {
                Ok(ty) => ty,
                Err(_) => return CoroutineState::Yielded(Sum::new(Async)),
            };
            // SAFETY: The reference is guaranteed to be valid for the span of this polling
            // lifetime.
            match fut.poll(unsafe { resume_ty.0.as_mut() }) {
                Poll::Ready(ret) => CoroutineState::Complete(ret),
                Poll::Pending => CoroutineState::Yielded(Sum::new(Async)),
            }
        })
    }
}

impl<F, T> Future for FutCoro<F>
where
    F: Future<Output = T>,
{
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        self.with(|fut| fut.poll(cx))
    }
}

pub enum FutureMarker {}

impl<F, T> IntoCoroutine<FutureMarker, ResumeAsync> for F
where
    F: IntoFuture<Output = T>,
{
    type Yield = EffectAsync;

    type Return = T;

    type IntoCoroutine = FutCoro<F::IntoFuture>;

    fn into_coroutine(self) -> Self::IntoCoroutine {
        FutCoro(self.into_future())
    }
}

pub async fn run<Coro, T>(coro: Coro) -> T
where
    Coro: Effectful<(Async, ()), Return = T>,
{
    fn poll<Coro, T>(mut coro: Pin<&mut Coro>, cx: &mut Context<'_>) -> Poll<T>
    where
        Coro: Effectful<(Async, ()), Return = T>,
    {
        // SAFETY: The reference is guaranteed to be valid for the span of this polling.
        let state = Sum::new(Async::tag(unsafe { ResumeTy::new(cx) }));
        match coro.as_mut().resume(state) {
            CoroutineState::Yielded(_) => Poll::Pending,
            CoroutineState::Complete(ret) => Poll::Ready(ret),
        }
    }

    let mut coro = core::pin::pin!(coro);
    poll_fn(|cx| poll(coro.as_mut(), cx)).await
}

#[cfg(test)]
mod test {
    use core::{
        future::Future,
        pin::{Pin, pin},
        task::{Context, Poll, Waker},
    };

    use crate::{effect::IntoCoroutine, future::run};

    #[test]
    fn test_async() {
        struct YieldNow(bool);

        impl Future for YieldNow {
            type Output = ();

            fn poll(mut self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<()> {
                if !self.0 {
                    self.0 = true;
                    cx.waker().wake_by_ref();
                    Poll::Pending
                } else {
                    Poll::Ready(())
                }
            }
        }

        let fut = async {
            YieldNow(false).await;
            42
        };

        let fut = fut;

        let coro = fut.into_coroutine();

        let mut run = pin!(run(coro));
        let mut cx = Context::from_waker(Waker::noop());

        assert_eq!(run.as_mut().poll(&mut cx), Poll::Pending);
        assert_eq!(run.as_mut().poll(&mut cx), Poll::Ready(42));
    }
}
