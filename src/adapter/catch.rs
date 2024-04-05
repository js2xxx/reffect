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
    effect::{EffectList, IntoCoroutine},
    util::{
        narrow_effect_prefixed,
        sum_type::{
            range::{ContainsList, SplitList},
            NarrowRem,
        },
        tag::U1,
        Sum,
    },
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

impl<Coro, Trans, Y, T, E, H, HY, OY, EUL, RemEUL, HOUL, OUL>
    Coroutine<Sum<(Begin, OY::ResumeList)>>
    for Catch<Coro, Trans, H, (Y, E, HY, OY), (EUL, RemEUL, HOUL, OUL)>
where
    Coro: Coroutine<Sum<(Begin, Y::ResumeList)>, Yield = Sum<Y>, Return = T>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<
        Sum<(Begin, HY::ResumeList)>,
        Yield = Sum<HY>,
        Return = ControlFlow<T, Sum<E::ResumeList>>,
    >,

    E: EffectList,
    Y: EffectList,
    HY: EffectList,
    OY: EffectList,
    NarrowRem<Y, E, EUL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

    Y: ContainsList<E, EUL, RemEUL>,
    Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL>,
    (Begin, Y::ResumeList): SplitList<Y::ResumeList, Y::Tags<U1>>,

    OY: SplitList<HY, HOUL> + SplitList<NarrowRem<Y, E, EUL>, OUL>,
    OY::ResumeList: SplitList<HY::ResumeList, HOUL>
        + SplitList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>, OUL>,
{
    type Yield = Sum<OY>;
    type Return = T;

    fn resume(
        self: Pin<&mut Self>,
        state: Sum<(Begin, OY::ResumeList)>,
    ) -> CoroutineState<Self::Yield, T> where {
        let mut proj = self.project();

        let mut state: Sum<(Begin, Y::ResumeList)> = match proj.handler.as_mut().as_pin_mut() {
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
