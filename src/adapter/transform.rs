use core::{
    convert::Infallible,
    marker::PhantomData,
    ops::{
        Coroutine,
        CoroutineState::{self, *},
    },
    pin::{pin, Pin},
};

use pin_project::pin_project;
use tuple_list::{Tuple, TupleList};

use crate::{
    adapter::{Begin, ResumeList},
    traits::IntoCoroutine,
    util::{
        narrow_effect,
        sum_type::{
            range::{TupleBirange, TupleRange},
            repr::{ReprMatch, TupleSum},
            NarrowRem,
        },
        tag::*,
        tuple::{ConcatList, ConcatT, ConcatTL},
        Sum,
    },
    Effect, EffectList, ResumeTuple, ResumeTy,
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

impl<Coro, Trans, R, Y, T, ER, E, H, HR, HY, OR, OY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL>
    Coroutine<Sum<OR>>
    for Transform<
        Coro,
        Trans,
        H,
        (R, Y, ER, E, HR, HY, OR, OY),
        (EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL),
    >
where
    Coro: Coroutine<Sum<R>, Yield = Sum<Y>, Return = T>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<Sum<HR>, Yield = Sum<HY>, Return = Sum<ER>>,

    R: TupleSum,
    R::TupleList: TupleList<Tuple = R>,
    Y: TupleSum,
    Y::TupleList: EffectList<ResumeList = R::TupleList, Tuple = Y>,

    ER: TupleSum,
    ER::TupleList: TupleList<Tuple = ER>,
    E: TupleSum,
    E::TupleList: EffectList<ResumeList = ER::TupleList, Tuple = E>,

    NarrowRem<R, ER, EUL>: TupleSum,
    <NarrowRem<R, ER, EUL> as Tuple>::TupleList: TupleList<Tuple = NarrowRem<R, ER, EUL>>,
    NarrowRem<Y, E, EUL>: TupleSum,
    <NarrowRem<Y, E, EUL> as Tuple>::TupleList: EffectList<
        ResumeList = <NarrowRem<R, ER, EUL> as Tuple>::TupleList,
        Tuple = NarrowRem<Y, E, EUL>,
    >,

    HR: TupleSum,
    HR::TupleList: TupleList<Tuple = HR>,
    HY: TupleSum,
    HY::TupleList: EffectList<ResumeList = HR::TupleList, Tuple = HY>,

    OR: TupleSum,
    OR::TupleList: TupleList<Tuple = OR>,
    OY: TupleSum,
    OY::TupleList: EffectList<ResumeList = OR::TupleList, Tuple = OY>,

    Y::TupleList: TupleBirange<E::TupleList, EUL, RemEUL>,
    R::TupleList: TupleBirange<ER::TupleList, EUL, RemEUL>,

    OY::TupleList: TupleRange<HY::TupleList, HOYUL>,
    OY::TupleList: TupleRange<<NarrowRem<Y, E, EUL> as Tuple>::TupleList, OYUL>,
    OR::TupleList: TupleRange<HR::TupleList, HORUL>,
    OR::TupleList: TupleRange<<NarrowRem<R, ER, EUL> as Tuple>::TupleList, ORUL>,

    R::Repr: ReprMatch<ResumeTy<Infallible>, U0>,
    HR::Repr: ReprMatch<ResumeTy<Infallible>, U0>,
{
    type Yield = Sum<OY>;
    type Return = T;

    fn resume(self: Pin<&mut Self>, state: Sum<OR>) -> CoroutineState<Self::Yield, T> {
        let mut proj = self.project();

        let mut state: Sum<R> = match proj.handler.as_mut().as_pin_mut() {
            Some(mut handler) => {
                let state = match handler
                    .as_mut()
                    .resume(narrow_effect(state, PhantomData::<HY>))
                {
                    Yielded(eff) => return Yielded(eff.broaden()),
                    Complete(ret) => ret.broaden(),
                };
                proj.handler.set(None);
                state
            }
            None => narrow_effect(state, PhantomData::<NarrowRem<Y, E, EUL>>).broaden(),
        };

        loop {
            match proj.coro.as_mut().resume(state) {
                Yielded(y) => {
                    state = match y.narrow::<E, _>() {
                        Ok(eff) => {
                            let handler = (proj.trans)(eff).into_coroutine();
                            proj.handler.set(Some(handler));

                            let handler = proj.handler.as_mut().as_pin_mut().unwrap();
                            match handler.resume(Sum::new(Infallible::tag(Begin))) {
                                Yielded(eff) => return Yielded(eff.broaden()),
                                Complete(ret) => ret.broaden(),
                            }
                        }
                        Err(rem) => break Yielded(rem.broaden()),
                    }
                }
                Complete(ret) => break Complete(ret),
            }
        }
    }
}

pub type Transform0<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HRUL, HYUL> = Transform<
    Coro,
    Trans,
    H,
    (
        ResumeTuple<Y>,
        Y,
        ResumeTuple<E>,
        E,
        ResumeTuple<HY>,
        HY,
        ResumeTuple<Y>,
        Y,
    ),
    (EUL, RemEUL, HRUL, RemEUL, HYUL, RemEUL),
>;

#[allow(clippy::type_complexity)]
pub fn transform0<Coro, Trans, Y, E, H, HY, EUL, RemEUL, HRUL, HYUL>(
    coro: Coro,
    trans: Trans,
) -> Transform0<Coro, Trans, H, Y, E, HY, EUL, RemEUL, HRUL, HYUL>
where
    Coro: Coroutine<Sum<ResumeTuple<Y>>, Yield = Sum<Y>>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<Sum<ResumeTuple<HY>>, Yield = Sum<HY>, Return = Sum<ResumeTuple<E>>>,

    ResumeTuple<Y>: TupleSum,
    Y: TupleSum,
    Y::TupleList: EffectList<Tuple = Y>,

    ResumeTuple<E>: TupleSum,
    E: TupleSum,
    E::TupleList: EffectList<Tuple = E>,

    ResumeTuple<HY>: TupleSum,
    HY: TupleSum,
    HY::TupleList: EffectList<Tuple = HY>,

    Y::TupleList: TupleBirange<E::TupleList, EUL, RemEUL>,
    ResumeList<Y>: TupleBirange<ResumeList<E>, EUL, RemEUL>,

    Y::TupleList: TupleRange<HY::TupleList, HYUL>,
    ResumeList<Y>: TupleRange<ResumeList<HY>, HRUL>,

    <ResumeTuple<Y> as TupleSum>::Repr: ReprMatch<ResumeTy<Infallible>, U0>,
    <ResumeTuple<HY> as TupleSum>::Repr: ReprMatch<ResumeTy<Infallible>, U0>,
{
    transform(coro, trans)
}

type CoprodList<A, B> = ConcatTL<A, B>;

type Coprod<A, B> = ConcatT<A, B>;

pub type Transform1<Coro, Trans, H, R, Y, E, ER, HR, HY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL> =
    Transform<
        Coro,
        Trans,
        H,
        (
            R,
            Y,
            ER,
            E,
            HR,
            HY,
            Coprod<HR, NarrowRem<R, ER, EUL>>,
            Coprod<HY, NarrowRem<Y, E, EUL>>,
        ),
        (EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL),
    >;

#[allow(clippy::type_complexity)]
pub fn transform1<Coro, Trans, R, Y, T, E, ER, H, HR, HY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL>(
    coro: Coro,
    trans: Trans,
) -> Transform1<Coro, Trans, H, R, Y, E, ER, HR, HY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL>
where
    Coro: Coroutine<Sum<R>, Yield = Sum<Y>, Return = T>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<Sum<HR>, Yield = Sum<HY>, Return = Sum<ER>>,

    R: TupleSum,
    R::TupleList: TupleList<Tuple = R>,
    Y: TupleSum,
    Y::TupleList: EffectList<ResumeList = R::TupleList, Tuple = Y>,

    ER: TupleSum,
    ER::TupleList: TupleList<Tuple = ER>,
    E: TupleSum,
    E::TupleList: EffectList<ResumeList = ER::TupleList, Tuple = E>,

    HR: TupleSum,
    HR::TupleList: TupleList<Tuple = HR>,
    HR::TupleList: ConcatList<<NarrowRem<R, ER, EUL> as Tuple>::TupleList>,
    HY: TupleSum,
    HY::TupleList: EffectList<ResumeList = HR::TupleList, Tuple = HY>,
    HY::TupleList: ConcatList<<NarrowRem<Y, E, EUL> as Tuple>::TupleList>,

    Coprod<HR, NarrowRem<R, ER, EUL>>: TupleSum,
    CoprodList<HR, NarrowRem<R, ER, EUL>>: TupleList,
    Coprod<HY, NarrowRem<Y, E, EUL>>: TupleSum,
    CoprodList<HY, NarrowRem<Y, E, EUL>>:
        EffectList<ResumeList = CoprodList<HR, NarrowRem<R, ER, EUL>>>,

    Y::TupleList: TupleBirange<E::TupleList, EUL, RemEUL>,
    R::TupleList: TupleBirange<ER::TupleList, EUL, RemEUL>,

    CoprodList<HY, NarrowRem<Y, E, EUL>>: TupleRange<HY::TupleList, HOYUL>,
    CoprodList<HY, NarrowRem<Y, E, EUL>>:
        TupleRange<<NarrowRem<Y, E, EUL> as Tuple>::TupleList, OYUL>,
    CoprodList<HR, NarrowRem<R, ER, EUL>>: TupleRange<HR::TupleList, HORUL>,
    CoprodList<HR, NarrowRem<R, ER, EUL>>:
        TupleRange<<NarrowRem<R, ER, EUL> as Tuple>::TupleList, ORUL>,

    R::Repr: ReprMatch<ResumeTy<Infallible>, U0>,
    HR::Repr: ReprMatch<ResumeTy<Infallible>, U0>,
{
    transform(coro, trans)
}
