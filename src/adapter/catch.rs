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
    adapter::Begin,
    traits::IntoCoroutine,
    util::{
        narrow_effect_prefixed, sum_type::{
            range::{TupleBirange, TupleRange},
            repr::TupleSum,
            NarrowRem,
        }, tag::U1, Sum
    },
    EffectList, PrefixedResumeList,
};

pub fn catch<Coro, Trans, H, MTypes, MULists>(
    coro: Coro,
    trans: Trans,
) -> Catch<Coro, Trans, H, MTypes, MULists> {
    Catch {
        coro,
        trans,
        handler: None,
        markers: PhantomData,
    }
}

#[pin_project]
pub struct Catch<Coro, Trans, H, MTypes, MULists> {
    #[pin]
    coro: Coro,
    trans: Trans,
    #[pin]
    handler: Option<H>,
    markers: PhantomData<(MTypes, MULists)>,
}

impl<Coro, Trans, Y, T, E, H, HY, OY, EUL, RemEUL, HOYUL, OYUL, HORUL, ORUL>
    Coroutine<Sum<PrefixedResumeList<OY>>>
    for Catch<Coro, Trans, H, (Y, E, HY, OY), (EUL, RemEUL, HOYUL, OYUL, HORUL, ORUL)>
where
    Coro: Coroutine<Sum<PrefixedResumeList<Y>>, Yield = Sum<Y>, Return = T>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<
        Sum<PrefixedResumeList<HY>>,
        Yield = Sum<HY>,
        Return = ControlFlow<T, Sum<E::ResumeList>>,
    >,

    E: EffectList,

    PrefixedResumeList<Y>: TupleSum,
    Y: EffectList,

    PrefixedResumeList<HY>: TupleSum,
    HY: EffectList,

    PrefixedResumeList<OY>: TupleSum,
    OY: EffectList,

    NarrowRem<Y, E, EUL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

    Y: TupleRange<E, EUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, EUL, RemEUL>,
    PrefixedResumeList<Y>: TupleRange<Y::ResumeList, Y::Tags<U1>>,

    OY: TupleRange<HY, HOYUL> + TupleRange<NarrowRem<Y, E, EUL>, OYUL>,
    OY::ResumeList: TupleRange<HY::ResumeList, HORUL>
        + TupleRange<<NarrowRem<Y, E, EUL> as EffectList>::ResumeList, ORUL>,
{
    type Yield = Sum<OY>;
    type Return = T;

    fn resume(
        self: Pin<&mut Self>,
        state: Sum<PrefixedResumeList<OY>>,
    ) -> CoroutineState<Self::Yield, T> where {
        let mut proj = self.project();

        let mut state: Sum<PrefixedResumeList<Y>> = match proj.handler.as_mut().as_pin_mut() {
            Some(mut handler) => {
                let state: Sum<Y::ResumeList> = match handler
                    .as_mut()
                    .resume(narrow_effect_prefixed(state, PhantomData::<HY>))
                {
                    Yielded(eff) => return Yielded(eff.broaden()),
                    Complete(Continue(ret)) => ret.broaden(),
                    Complete(Break(ret)) => return Complete(ret),
                };
                proj.handler.set(None);
                state.broaden()
            }
            None => narrow_effect_prefixed(state, PhantomData::<NarrowRem<Y, E, EUL>>).broaden(),
        };

        loop {
            match proj.coro.as_mut().resume(state) {
                Yielded(y) => {
                    state = match y.narrow() {
                        Ok(eff) => {
                            let handler = (proj.trans)(eff).into_coroutine();
                            proj.handler.set(Some(handler));

                            let handler = proj.handler.as_mut().as_pin_mut().unwrap();
                            let state: Sum<Y::ResumeList> = match handler.resume(Sum::new(Begin)) {
                                Yielded(eff) => break Yielded(eff.broaden()),
                                Complete(Continue(ret)) => ret.broaden(),
                                Complete(Break(ret)) => break Complete(ret),
                            };
                            proj.handler.set(None);
                            state.broaden()
                        }
                        Err(rem) => break Yielded(rem.broaden()),
                    }
                }
                Complete(ret) => break Complete(ret),
            }
        }
    }
}
