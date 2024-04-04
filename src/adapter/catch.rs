use core::{
    convert::Infallible,
    marker::PhantomData,
    ops::{
        ControlFlow::{self, *}, Coroutine, CoroutineState::{self, *}
    },
    pin::{pin, Pin},
};

use pin_project::pin_project;
use tuple_list::{Tuple, TupleList};

use crate::{
    adapter::Begin,
    traits::IntoCoroutine,
    util::{
        narrow_effect,
        sum_type::{
            range::{TupleBirange, TupleRange},
            repr::{ReprMatch, TupleSum},
            NarrowRem,
        },
        tag::*,
        Sum,
    },
    Effect, EffectList, ResumeTy,
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

impl<Coro, Trans, R, Y, T, ER, E, H, HR, HY, OR, OY, EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL>
    Coroutine<Sum<OR>>
    for Catch<
        Coro,
        Trans,
        H,
        (R, Y, ER, E, HR, HY, OR, OY),
        (EUL, RemEUL, HORUL, ORUL, HOYUL, OYUL),
    >
where
    Coro: Coroutine<Sum<R>, Yield = Sum<Y>, Return = T>,
    Trans: FnMut(Sum<E>) -> H,
    H: Coroutine<Sum<HR>, Yield = Sum<HY>, Return = ControlFlow<T, Sum<ER>>>,

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
                    Complete(Continue(ret)) => ret.broaden(),
                    Complete(Break(ret)) => return Complete(ret),
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
                                Yielded(eff) => break Yielded(eff.broaden()),
                                Complete(Continue(ret)) => ret.broaden(),
                                Complete(Break(ret)) => break Complete(ret),
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