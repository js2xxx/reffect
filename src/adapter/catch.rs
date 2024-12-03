use core::{
    marker::{PhantomData, PhantomPinned},
    ops::{
        ControlFlow::*,
        Coroutine,
        CoroutineState::{self, *},
    },
    pin::{Pin, pin},
};

use pin_project::pin_project;

use crate::{
    Effectful,
    adapter::Begin,
    effect::{Catcher, EffectList},
    util::{
        ConcatList, Sum, narrow_effect_prefixed,
        sum_type::{
            NarrowRem,
            range::{ContainsList, Count, SplitList},
            repr::SumList,
        },
        tag::{U1, UTerm},
    },
};

pub fn catch<'h, Coro, C, CM, Y, E, HY, OY, MULists>(
    coro: Coro,
    catcher: C,
) -> Catch<'h, Coro, C, CM, Y, E, HY, OY, MULists>
where
    Coro: Effectful<Y>,
    C: Catcher<Coro::Return, E, HY, CM> + 'h,
    E: EffectList,
    Y: EffectList,
    HY: EffectList,
{
    Catch {
        coro,
        catcher,
        handler: None,
        markers: PhantomData,
        pinned: PhantomPinned,
    }
}

#[pin_project]
pub struct Catch<'h, Coro, C, CM, Y, E, HY, OY, MULists>
where
    Coro: Effectful<Y>,
    C: Catcher<Coro::Return, E, HY, CM> + 'h,
    E: EffectList,
    Y: EffectList,
    HY: EffectList,
{
    #[pin]
    coro: Coro,
    // `handler` must be dropped before `catcher` since it is actually borrowed from it.
    //
    // Note: struct fields are dropped in the same order as declared in the struct
    // (source-code-wise, not memory-layout-wise).
    #[pin]
    handler: Option<C::Catcher<'h>>,
    #[pin]
    catcher: C,
    markers: PhantomData<(Y, E, HY, OY, MULists)>,
    // This struct must be pinned, since it contains a self-referential field (`handler`).
    pinned: PhantomPinned,
}

impl<'h, Coro, C, CM, Y, E, HY, OY, EUL, RemEUL, HOUL, OUL> Coroutine<Sum<(Begin, OY::ResumeList)>>
    for Catch<'h, Coro, C, CM, Y, E, HY, OY, (EUL, RemEUL, HOUL, OUL)>
where
    Coro: Effectful<Y>,
    C: Catcher<Coro::Return, E, HY, CM> + 'h,
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
                // If there's a handler, we must not access `catcher`.
                let state: Sum<Y::ResumeList> = match handler
                    .as_mut()
                    .resume(narrow_effect_prefixed(state, PhantomData::<HY>))
                {
                    Yielded(eff) => return Yielded(eff.broaden()),
                    Complete(Continue(ret)) => ret.broaden(),
                    Complete(Break(ret)) => return Complete(ret),
                };
                // We may access `catcher` in the following code, so `handler` must be dropped
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
                        // SAFETY: The handler is actually mutably borrowed from `catcher`, which
                        // indicates that:
                        //
                        // 1. `catcher` must not be dropped before `handler`.
                        // 2. `catcher` must not be accessed when `handler` is alive in the scope.
                        //
                        // Thus, we can safely extend its lifetime to almost `'h`.
                        let handler: C::Catcher<'h> =
                            unsafe { core::mem::transmute(proj.catcher.as_mut().catch(eff)) };
                        proj.handler.set(Some(handler));

                        let handler = proj.handler.as_mut().as_pin_mut().unwrap();
                        let state: Sum<Y::ResumeList> = match handler.resume(Sum::new(Begin)) {
                            Yielded(eff) => break Yielded(eff.broaden()),
                            Complete(Continue(ret)) => ret.broaden(),
                            Complete(Break(ret)) => break Complete(ret),
                        };
                        // We may access `catcher` in the next iteration of the loop, so `handler`
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

pub type Catch0<'h, Coro, C, CM, Y, E, HY, EUL, RemEUL, HUL> =
    Catch<'h, Coro, C, CM, Y, E, HY, Y, (EUL, RemEUL, HUL, RemEUL)>;

pub fn catch0<'h, Coro, C, CM, E, Y, HY, EUL, RemEUL, HUL>(
    coro: Coro,
    catcher: C,
) -> Catch0<'h, Coro, C, CM, Y, E, HY, EUL, RemEUL, HUL>
where
    Coro: Effectful<Y>,
    C: Catcher<Coro::Return, E, HY, CM> + 'h,
    E: EffectList,
    HY: EffectList,
    Y: EffectList + ContainsList<E, EUL, RemEUL> + SplitList<HY, HUL>,
    Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL> + SplitList<HY::ResumeList, HUL>,
{
    catch(coro, catcher)
}

pub type Catch1<'h, Coro, C, CM, Y, E, HY, EUL, RemEUL> = Catch<
    'h,
    Coro,
    C,
    CM,
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

pub fn catch1<'h, Coro, C, CM, E, Y, HY, EUL, RemEUL>(
    coro: Coro,
    catcher: C,
) -> Catch1<'h, Coro, C, CM, Y, E, HY, EUL, RemEUL>
where
    Coro: Effectful<Y>,
    C: Catcher<Coro::Return, E, HY, CM> + 'h,
    E: EffectList,
    HY: EffectList + ConcatList<NarrowRem<Y, E, EUL>>,
    HY::ResumeList: ConcatList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,
    Y: EffectList + ContainsList<E, EUL, RemEUL>,
    Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL>,
{
    catch(coro, catcher)
}
