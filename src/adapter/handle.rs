use core::{
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
            range::{TupleBirange, TupleRange},
            NarrowRem,
        },
        tag::U1,
        Sum,
    },
    EffectList, PrefixedResumeList,
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

impl<Coro, Y, T, H, E, RemUL, UL> Coroutine<Sum<PrefixedResumeList<NarrowRem<Y, E, UL>>>>
    for Handle<Coro, H, (Y, T, E, RemUL, UL)>
where
    Coro: Coroutine<Sum<PrefixedResumeList<Y>>, Yield = Sum<Y>, Return = T>,
    H: FnMut(Sum<E>) -> ControlFlow<T, Sum<E::ResumeList>>,

    Y: EffectList,
    E: EffectList,

    NarrowRem<Y, E, UL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, UL>>,

    Y: TupleBirange<E, UL, RemUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, UL, RemUL>,
    PrefixedResumeList<Y>: TupleRange<Y::ResumeList, Y::Tags<U1>>,
{
    type Yield = Sum<NarrowRem<Y, E, UL>>;
    type Return = T;

    fn resume(
        self: Pin<&mut Self>,
        state: Sum<PrefixedResumeList<NarrowRem<Y, E, UL>>>,
    ) -> CoroutineState<Self::Yield, T> {
        let mut state = state.broaden();
        let mut proj = self.project();
        loop {
            match proj.coro.as_mut().resume(state) {
                Yielded(yielded) => {
                    let r: Sum<Y::ResumeList> = match yielded.narrow() {
                        Ok(narrowed) => match (proj.handler)(narrowed) {
                            Break(ret) => break Complete(ret),
                            Continue(r) => r.broaden(),
                        },
                        Err(y) => break Yielded(y),
                    };
                    state = r.broaden()
                }
                Complete(ret) => break Complete(ret),
            }
        }
    }
}
