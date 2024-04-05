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
        narrow_effect_prefixed,
        sum_type::{
            range::{TupleBirange, TupleCount, TupleRange},
            repr::TupleSum,
            NarrowRem,
        },
        tag::{UTerm, U1},
        tuple::ConcatList,
        Sum,
    },
    EffectList, Effectful,
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

impl<Coro, Trans, Y, E, H, HY, OY, EUL, RemEUL, HOUL, OUL> Coroutine<Sum<(Begin, OY::ResumeList)>>
    for Transform<Coro, Trans, H, (Y, E, HY, OY), (EUL, RemEUL, HOUL, OUL)>
where
    Coro: Effectful<Y>,
    Trans: FnMut(Sum<E>) -> H,
    H: Effectful<HY, Return = Sum<E::ResumeList>>,

    E: EffectList,
    Y: EffectList,
    HY: EffectList,
    OY: EffectList,
    NarrowRem<Y, E, EUL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

    Y: TupleBirange<E, EUL, RemEUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, EUL, RemEUL>,
    (Begin, Y::ResumeList): TupleRange<Y::ResumeList, Y::Tags<U1>>,

    OY: TupleRange<HY, HOUL> + TupleRange<NarrowRem<Y, E, EUL>, OUL>,
    OY::ResumeList: TupleRange<HY::ResumeList, HOUL>
        + TupleRange<NarrowRem<Y::ResumeList, E::ResumeList, EUL>, OUL>,
{
    type Yield = Sum<OY>;
    type Return = Coro::Return;

    fn resume(
        self: Pin<&mut Self>,
        state: Sum<(Begin, OY::ResumeList)>,
    ) -> CoroutineState<Self::Yield, Self::Return> {
        let mut proj = self.project();

        let mut state: Sum<(Begin, Y::ResumeList)> = match proj.handler.as_mut().as_pin_mut() {
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

pub type Transform0<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HUL> =
    Transform<Coro, Trans, H, (Y, E, HY, Y), (EUL, RemEUL, HUL, RemEUL)>;

pub fn transform0<Coro, Trans, Y, E, H, HY, EUL, RemEUL, HUL>(
    coro: Coro,
    trans: Trans,
) -> Transform0<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HUL>
where
    Coro: Effectful<Y>,
    Trans: FnMut(Sum<E>) -> H,
    H: Effectful<HY, Return = Sum<E::ResumeList>>,

    E: EffectList,
    HY: EffectList,

    Y: EffectList + TupleBirange<E, EUL, RemEUL> + TupleRange<HY, HUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, EUL, RemEUL> + TupleRange<HY::ResumeList, HUL>,
{
    transform(coro, trans)
}

pub type Transform1<Coro, Trans, H, Y, E, HY, EUL, RemEUL> = Transform<
    Coro,
    Trans,
    H,
    (Y, E, HY, <HY as ConcatList<NarrowRem<Y, E, EUL>>>::Output),
    (
        EUL,
        RemEUL,
        <HY as TupleSum>::Tags<UTerm>,
        <NarrowRem<Y, E, EUL> as TupleSum>::Tags<<HY as TupleCount>::Count>,
    ),
>;

pub fn transform1<Coro, Trans, Y, E, H, HY, EUL, RemEUL>(
    coro: Coro,
    trans: Trans,
) -> Transform1<Coro, Trans, H, Y, E, HY, EUL, RemEUL>
where
    Coro: Effectful<Y>,
    Trans: FnMut(Sum<E>) -> H,
    H: Effectful<HY, Return = Sum<E::ResumeList>>,

    E: EffectList,
    HY: EffectList + ConcatList<NarrowRem<Y, E, EUL>>,
    HY::ResumeList: ConcatList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

    Y: EffectList + TupleBirange<E, EUL, RemEUL>,
    Y::ResumeList: TupleBirange<E::ResumeList, EUL, RemEUL>,
{
    transform(coro, trans)
}
