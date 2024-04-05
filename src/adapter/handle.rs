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

use super::Begin;
use crate::{
    util::{
        sum_type::{
            range::{TupleBirange, TupleRange},
            NarrowRem,
        },
        tag::U1,
        Sum,
    },
    EffectList, Effectful,
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

impl<Coro, Y, H, E, RemUL, UL> Coroutine<Sum<(Begin, NarrowRem<Y::ResumeList, E::ResumeList, UL>)>>
    for Handle<Coro, H, (Y, E, RemUL, UL)>
where
    Coro: Effectful<Y>,
    H: FnMut(Sum<E>) -> ControlFlow<Coro::Return, Sum<E::ResumeList>>,

    Y: EffectList,
    E: EffectList,

    NarrowRem<Y, E, UL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, UL>>,

    Y: TupleBirange<E, UL, RemUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, UL, RemUL>,
    (Begin, Y::ResumeList): TupleRange<Y::ResumeList, Y::Tags<U1>>,
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
                        Ok(narrowed) => match (proj.handler)(narrowed) {
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
