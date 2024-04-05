use core::{
    marker::PhantomData,
    ops::{
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
        }, tag::U1, tuple::{ConcatList, ConcatT}, Sum
    },
    EffectList, PrefixedResumeList,
};

pub fn transform<Coro, Trans, H, MTypes, MULists>(
    coro: Coro,
    trans: Trans,
) -> Transform<Coro, Trans, H, MTypes, MULists> {
    Transform {
        coro,
        trans,
        handler: None,
        markers: PhantomData,
    }
}

#[pin_project]
pub struct Transform<Coro, Trans, H, MTypes, MULists> {
    #[pin]
    coro: Coro,
    trans: Trans,
    #[pin]
    handler: Option<H>,
    markers: PhantomData<(MTypes, MULists)>,
}

impl<Coro, Trans, Y, T, E, H, HY, OY, EUL, RemEUL, HOYUL, OYUL, HORUL, ORUL>
    Coroutine<Sum<PrefixedResumeList<OY>>>
    for Transform<Coro, Trans, H, (Y, E, HY, OY), (EUL, RemEUL, HOYUL, OYUL, HORUL, ORUL)>
where
    Coro: Coroutine<Sum<PrefixedResumeList<Y>>, Yield = Sum<Y>, Return = T>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<Sum<PrefixedResumeList<HY>>, Yield = Sum<HY>, Return = Sum<E::ResumeList>>,

    E: EffectList,

    PrefixedResumeList<Y>: TupleSum,
    Y: EffectList,

    PrefixedResumeList<HY>: TupleSum,
    HY: EffectList,

    PrefixedResumeList<OY>: TupleSum,
    OY: EffectList,

    NarrowRem<Y, E, EUL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

    Y: TupleBirange<E, EUL, RemEUL>,
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
    ) -> CoroutineState<Self::Yield, T> {
        let mut proj = self.project();

        let mut state: Sum<PrefixedResumeList<Y>> = match proj.handler.as_mut().as_pin_mut() {
            Some(mut handler) => {
                let state: Sum<Y::ResumeList> = match handler
                    .as_mut()
                    .resume(narrow_effect_prefixed(state, PhantomData::<HY>))
                {
                    Yielded(eff) => return Yielded(eff.broaden()),
                    Complete(ret) => ret.broaden(),
                };
                proj.handler.set(None);
                state.broaden()
            }
            None => narrow_effect_prefixed(state, PhantomData::<NarrowRem<Y, E, EUL>>).broaden(),
        };

        loop {
            match proj.coro.as_mut().resume(state) {
                Yielded(y) => {
                    state = match y.narrow::<E, _>() {
                        Ok(eff) => {
                            let handler = (proj.trans)(eff).into_coroutine();
                            proj.handler.set(Some(handler));

                            let handler = proj.handler.as_mut().as_pin_mut().unwrap();
                            let state: Sum<Y::ResumeList> = match handler.resume(Sum::new(Begin)) {
                                Yielded(eff) => break Yielded(eff.broaden()),
                                Complete(ret) => ret.broaden(),
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

pub type Transform0<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HRUL, HYUL> =
    Transform<Coro, Trans, H, (Y, E, HY, Y), (EUL, RemEUL, HRUL, RemEUL, HYUL, RemEUL)>;

#[allow(clippy::type_complexity)]
pub fn transform0<Coro, Trans, Y, E, H, HY, EUL, RemEUL, HRUL, HYUL>(
    coro: Coro,
    trans: Trans,
) -> Transform0<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HRUL, HYUL>
where
    Coro: Coroutine<Sum<PrefixedResumeList<Y>>, Yield = Sum<Y>>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<Sum<PrefixedResumeList<HY>>, Yield = Sum<HY>, Return = Sum<E::ResumeList>>,

    E: EffectList,

    PrefixedResumeList<Y>: TupleSum,
    Y: EffectList,

    PrefixedResumeList<HY>: TupleSum,
    HY: EffectList,

    Y: TupleBirange<E, EUL, RemEUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, EUL, RemEUL>,

    Y: TupleRange<HY, HYUL>,
    Y::ResumeList: TupleRange<HY::ResumeList, HRUL>,
{
    transform(coro, trans)
}

type Coprod<A, B> = ConcatT<A, B>;

pub type Transform1<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL> = Transform<
    Coro,
    Trans,
    H,
    (Y, E, HY, Coprod<HY, NarrowRem<Y, E, EUL>>),
    (EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL),
>;

#[allow(clippy::type_complexity)]
pub fn transform1<Coro, Trans, Y, T, E, H, HY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL>(
    coro: Coro,
    trans: Trans,
) -> Transform1<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL>
where
    Coro: Coroutine<Sum<PrefixedResumeList<Y>>, Yield = Sum<Y>, Return = T>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<Sum<PrefixedResumeList<HY>>, Yield = Sum<HY>, Return = Sum<E::ResumeList>>,

    E: EffectList,

    PrefixedResumeList<Y>: TupleSum,
    Y: EffectList,

    PrefixedResumeList<HY>: TupleSum,
    HY: EffectList,

    HY::ResumeList: ConcatList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,
    HY: ConcatList<NarrowRem<Y, E, EUL>>,

    Coprod<HY::ResumeList, NarrowRem<Y::ResumeList, E::ResumeList, EUL>>: TupleSum,
    Coprod<HY, NarrowRem<Y, E, EUL>>: EffectList<
        ResumeList = Coprod<HY::ResumeList, NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,
    >,

    Y: TupleBirange<E, EUL, RemEUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, EUL, RemEUL>,

    Coprod<HY, NarrowRem<Y, E, EUL>>: TupleRange<HY, HOYUL>,
    Coprod<HY, NarrowRem<Y, E, EUL>>: TupleRange<NarrowRem<Y, E, EUL>, OYUL>,
    Coprod<HY::ResumeList, NarrowRem<Y::ResumeList, E::ResumeList, EUL>>:
        TupleRange<HY::ResumeList, HORUL>,
    Coprod<HY::ResumeList, NarrowRem<Y::ResumeList, E::ResumeList, EUL>>:
        TupleRange<NarrowRem<Y::ResumeList, E::ResumeList, EUL>, ORUL>,
{
    transform(coro, trans)
}
