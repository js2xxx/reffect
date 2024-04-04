use core::{
    convert::Infallible,
    marker::PhantomData,
    ops::{
        ControlFlow::{self, *},
        Coroutine,
        CoroutineState::{self, *},
    },
    pin::{pin, Pin},
};

use pin_project::pin_project;

use crate::{
    util::{
        sum_type::{
            range::TupleBirange,
            repr::{ReprMatch, TupleSum},
            NarrowRem,
        },
        tag::*,
        Sum,
    },
    EffectList, ResumeTuple, ResumeTy,
};

pub fn handle<Coro, H, Markers>(coro: Coro, handler: H) -> Handle<Coro, H, Markers> {
    Handle {
        coro,
        handler,
        markers: PhantomData,
    }
}

#[derive(Debug, Clone, Copy)]
#[pin_project]
pub struct Handle<Coro, H, Markers> {
    #[pin]
    coro: Coro,
    handler: H,
    markers: PhantomData<Markers>,
}

impl<Coro, Y, T, H, E, RemUL, UL> Coroutine<Sum<NarrowRem<ResumeTuple<Y>, ResumeTuple<E>, UL>>>
    for Handle<Coro, H, (Y, T, E, RemUL, UL)>
where
    Coro: Coroutine<Sum<ResumeTuple<Y>>, Yield = Sum<Y>, Return = T>,
    H: FnMut(Sum<E>) -> ControlFlow<T, Sum<ResumeTuple<E>>>,

    ResumeTuple<Y>: TupleSum,
    Y: TupleSum,
    Y::TupleList: EffectList,

    ResumeTuple<E>: TupleSum,
    E: TupleSum,
    E::TupleList: EffectList,

    <Y::TupleList as EffectList>::ResumeList:
        TupleBirange<<E::TupleList as EffectList>::ResumeList, UL, RemUL>,
    Y::TupleList: TupleBirange<E::TupleList, UL, RemUL, Tuple = Y>,

    <ResumeTuple<Y> as TupleSum>::Repr: ReprMatch<ResumeTy<Infallible>, U0>,
{
    type Yield = Sum<NarrowRem<Y, E, UL>>;
    type Return = T;

    fn resume(
        self: Pin<&mut Self>,
        state: Sum<NarrowRem<ResumeTuple<Y>, ResumeTuple<E>, UL>>,
    ) -> CoroutineState<Self::Yield, T> {
        let mut state = state.broaden();
        let mut proj = self.project();
        loop {
            match proj.coro.as_mut().resume(state) {
                Yielded(yielded) => {
                    state = match yielded.narrow() {
                        Ok(narrowed) => match (proj.handler)(narrowed) {
                            Break(ret) => break Complete(ret),
                            Continue(r) => r.broaden(),
                        },
                        Err(y) => break Yielded(y),
                    }
                }
                Complete(ret) => break Complete(ret),
            }
        }
    }
}
