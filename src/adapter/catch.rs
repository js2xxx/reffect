use core::{
    marker::{PhantomData, PhantomPinned},
    ops::{
        ControlFlow::*,
        Coroutine,
        CoroutineState::{self, *},
    },
    pin::{pin, Pin},
};

use pin_project::pin_project;

use crate::{
    adapter::Begin,
    effect::{EffectList, Handler},
    util::{
        narrow_effect_prefixed,
        sum_type::{
            range::{ContainsList, Count, SplitList},
            repr::SumList,
            NarrowRem,
        },
        tag::{UTerm, U1},
        ConcatList, Sum,
    },
    Effectful,
};

pub fn catch<'h, Coro, Trans, Y, E, HY, OY, MULists>(
    coro: Coro,
    trans: Trans,
) -> Catch<'h, Coro, Trans, Y, E, HY, OY, MULists>
where
    Coro: Effectful<Y>,
    Trans: Handler<Coro::Return, E, HY> + 'h,

    E: EffectList,
    Y: EffectList,
    HY: EffectList,
{
    Catch {
        coro,
        trans,
        handler: None,
        markers: PhantomData,
        pinned: PhantomPinned,
    }
}

#[pin_project]
pub struct Catch<'h, Coro, Trans, Y, E, HY, OY, MULists>
where
    Coro: Effectful<Y>,
    Trans: Handler<Coro::Return, E, HY> + 'h,

    E: EffectList,
    Y: EffectList,
    HY: EffectList,
{
    #[pin]
    coro: Coro,
    // `handler` must be dropped before `trans` since it is actually borrowed from it.
    //
    // Note: struct fields are dropped in the same order as declared in the struct
    // (source-code-wise, not memory-layout-wise).
    #[pin]
    handler: Option<Trans::Handler<'h>>,
    trans: Trans,
    markers: PhantomData<(Y, E, HY, OY, MULists)>,
    // This struct must be pinned, since it contains a self-referential field (`handler`).
    pinned: PhantomPinned,
}

impl<'h, Coro, Trans, Y, E, HY, OY, EUL, RemEUL, HOUL, OUL> Coroutine<Sum<(Begin, OY::ResumeList)>>
    for Catch<'h, Coro, Trans, Y, E, HY, OY, (EUL, RemEUL, HOUL, OUL)>
where
    Coro: Effectful<Y>,
    Trans: Handler<Coro::Return, E, HY> + 'h,

    E: EffectList,
    HY: EffectList,
    NarrowRem<Y, E, EUL>: EffectList<ResumeList = NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

    Y: EffectList + ContainsList<E, EUL, RemEUL>,
    Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL>,
    (Begin, Y::ResumeList): SplitList<Y::ResumeList, Y::Tags<U1>>,

    OY: EffectList + SplitList<HY, HOUL> + SplitList<NarrowRem<Y, E, EUL>, OUL>,
    OY::ResumeList: SplitList<HY::ResumeList, HOUL>
        + SplitList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>, OUL>,
{
    type Yield = Sum<OY>;
    type Return = Coro::Return;

    fn resume(
        self: Pin<&mut Self>,
        state: Sum<(Begin, OY::ResumeList)>,
    ) -> CoroutineState<Sum<OY>, Coro::Return> {
        let mut proj = self.project();

        let mut state: Sum<(Begin, Y::ResumeList)> = match proj.handler.as_mut().as_pin_mut() {
            Some(mut handler) => {
                // If there's a handler, we must not access `trans`.
                let state: Sum<Y::ResumeList> = match handler
                    .as_mut()
                    .resume(narrow_effect_prefixed(state, PhantomData::<HY>))
                {
                    Yielded(eff) => return Yielded(eff.broaden()),
                    Complete(Continue(ret)) => ret.broaden(),
                    Complete(Break(ret)) => return Complete(ret),
                };
                // We may access `trans` in the following code, so `handler` must be dropped
                // first.
                proj.handler.set(None);
                state.broaden()
            }
            None => narrow_effect_prefixed(state, PhantomData::<NarrowRem<Y, E, EUL>>).broaden(),
        };

        loop {
            state = match proj.coro.as_mut().resume(state) {
                Yielded(y) => match y.narrow() {
                    Ok(eff) => {
                        // SAFETY: The handler is actually mutably borrowed from `trans`, which
                        // indicates that:
                        //
                        // 1. `trans` must not be dropped before `handler`.
                        // 2. `trans` must not be accessed when `handler` is alive in the scope.
                        //
                        // Thus, we can safely extend its lifetime to almost `'h`.
                        let handler: Trans::Handler<'h> =
                            unsafe { core::mem::transmute(proj.trans.handle(eff)) };
                        proj.handler.set(Some(handler));

                        let handler = proj.handler.as_mut().as_pin_mut().unwrap();
                        let state: Sum<Y::ResumeList> = match handler.resume(Sum::new(Begin)) {
                            Yielded(eff) => break Yielded(eff.broaden()),
                            Complete(Continue(ret)) => ret.broaden(),
                            Complete(Break(ret)) => break Complete(ret),
                        };
                        // We may access `trans` in the next iteration of the loop, so `handler`
                        // must be dropped first.
                        proj.handler.set(None);
                        state.broaden()
                    }
                    Err(rem) => break Yielded(rem.broaden()),
                },
                Complete(ret) => break Complete(ret),
            }
        }
    }
}

pub type Catch0<'h, Coro, Trans, Y, E, HY, EUL, RemEUL, HUL> =
    Catch<'h, Coro, Trans, Y, E, HY, Y, (EUL, RemEUL, HUL, RemEUL)>;

pub fn catch0<'h, Coro, Trans, E, Y, HY, EUL, RemEUL, HUL>(
    coro: Coro,
    trans: Trans,
) -> Catch0<'h, Coro, Trans, Y, E, HY, EUL, RemEUL, HUL>
where
    Coro: Effectful<Y>,
    Trans: Handler<Coro::Return, E, HY> + 'h,

    E: EffectList,
    HY: EffectList,

    Y: EffectList + ContainsList<E, EUL, RemEUL> + SplitList<HY, HUL>,
    Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL> + SplitList<HY::ResumeList, HUL>,
{
    catch(coro, trans)
}

pub type Catch1<'h, Coro, Trans, Y, E, HY, EUL, RemEUL> = Catch<
    'h,
    Coro,
    Trans,
    Y,
    E,
    HY,
    <HY as ConcatList<NarrowRem<Y, E, EUL>>>::Output,
    (
        EUL,
        RemEUL,
        <HY as SumList>::Tags<UTerm>,
        <NarrowRem<Y, E, EUL> as SumList>::Tags<<HY as Count>::Count>,
    ),
>;

pub fn catch1<'h, Coro, Trans, E, Y, HY, EUL, RemEUL>(
    coro: Coro,
    trans: Trans,
) -> Catch1<'h, Coro, Trans, Y, E, HY, EUL, RemEUL>
where
    Coro: Effectful<Y>,
    Trans: Handler<Coro::Return, E, HY> + 'h,

    E: EffectList,
    HY: EffectList + ConcatList<NarrowRem<Y, E, EUL>>,
    HY::ResumeList: ConcatList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

    Y: EffectList + ContainsList<E, EUL, RemEUL>,
    Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL>,
{
    catch(coro, trans)
}
