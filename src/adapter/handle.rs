use core::{
    marker::PhantomData,
    ops::{
        ControlFlow::*,
        Coroutine,
        CoroutineState::{self, *},
    },
    pin::{Pin, pin},
};

use pin_project::pin_project;

use super::Begin;
use crate::{
    effect::{EffectList, Effectful, Handler},
    util::{
        Sum,
        sum_type::{
            NarrowRem,
            range::{ContainsList, SplitList},
        },
        tag::U1,
    },
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
    #[pin]
    handler: H,
    markers: PhantomData<Markers>,
}

impl<Coro, Y, H, HM, E, RemUL, UL>
    Coroutine<Sum<(Begin, NarrowRem<Y::ResumeList, E::ResumeList, UL>)>>
    for Handle<Coro, H, (HM, Y, E, RemUL, UL)>
where
    Coro: Effectful<Y>,
    H: Handler<Coro::Return, E, HM>,
    Y: EffectList + ContainsList<E, UL, RemUL>,
    E: EffectList,
    NarrowRem<Y, E, UL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, UL>>,
    Y::ResumeList: ContainsList<E::ResumeList, UL, RemUL>,
    (Begin, Y::ResumeList): SplitList<Y::ResumeList, Y::Tags<U1>>,
{
    type Yield = Sum<NarrowRem<Y, E, UL>>;
    type Return = Coro::Return;

    fn resume(
        self: Pin<&mut Self>,
        state: Sum<(Begin, NarrowRem<Y::ResumeList, E::ResumeList, UL>)>,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        let mut state = state.broaden();
        let mut proj = self.project();
        loop {
            match proj.coro.as_mut().resume(state) {
                Yielded(yielded) => {
                    let r: Sum<Y::ResumeList> = match yielded.narrow() {
                        Ok(narrowed) => match proj.handler.as_mut().handle(narrowed) {
                            Continue(r) => r.broaden(),
                            Break(ret) => break Complete(ret),
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
