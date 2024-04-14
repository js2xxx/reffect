mod catch;
mod handle;

use core::{
    ops::{Coroutine, CoroutineState::*},
    pin::pin,
};

pub use self::{
    catch::{catch, catch0, catch1, Catch, Catch0, Catch1},
    handle::{handle, Handle},
};
use crate::{
    effect::{Catcher, EffectList, Effectful},
    util::{
        sum_type::{
            range::{ContainsList, SplitList},
            NarrowRem,
        },
        ConcatList, Sum,
    },
};

#[derive(Debug)]
pub struct Begin;

pub fn run<Coro: Effectful>(coro: Coro) -> Coro::Return {
    match pin!(coro).resume(Sum::new(Begin)) {
        Complete(ret) => ret,
        Yielded(eff) => eff.unreachable(),
    }
}

pub trait EffectfulExt<Y: EffectList>: Effectful<Y> + Sized {
    fn run(self) -> <Self as Coroutine<Sum<(Begin, ())>>>::Return
    where
        Self: Effectful + Coroutine<Sum<(Begin, ())>>,
    {
        run(self)
    }

    fn handle<H, Markers>(self, handler: H) -> Handle<Self, H, Markers> {
        handle(self, handler)
    }

    fn catch<'h, C, CM, E, HY, OY, MULists>(
        self,
        catcher: C,
    ) -> Catch<'h, Self, C, CM, Y, E, HY, OY, MULists>
    where
        C: Catcher<Self::Return, E, HY, CM> + 'h,

        E: EffectList,
        Y: EffectList,
        HY: EffectList,
    {
        catch(self, catcher)
    }

    fn catch0<'h, C, CM, E, HY, EUL, RemEUL, HUL>(
        self,
        catcher: C,
    ) -> Catch0<'h, Self, C, CM, Y, E, HY, EUL, RemEUL, HUL>
    where
        C: Catcher<Self::Return, E, HY, CM> + 'h,

        E: EffectList,
        HY: EffectList,

        Y: EffectList + ContainsList<E, EUL, RemEUL> + SplitList<HY, HUL>,
        Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL> + SplitList<HY::ResumeList, HUL>,
    {
        catch(self, catcher)
    }

    fn catch1<'h, C, CM, E, HY, EUL, RemEUL>(
        self,
        catcher: C,
    ) -> Catch1<'h, Self, C, CM, Y, E, HY, EUL, RemEUL>
    where
        C: Catcher<Self::Return, E, HY, CM> + 'h,

        E: EffectList,
        HY: EffectList + ConcatList<NarrowRem<Y, E, EUL>>,
        HY::ResumeList: ConcatList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

        Y: EffectList + ContainsList<E, EUL, RemEUL>,
        Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL>,
    {
        catch(self, catcher)
    }
}

impl<F, E> EffectfulExt<E> for F
where
    E: EffectList,
    F: Effectful<E>,
{
}

#[cfg(test)]
mod test {
    use core::ops::Coroutine;
    use std::string::{String, ToString};

    use super::{Begin, EffectfulExt};
    use crate::{
        self as reffect, effectful, effectful_block, handler, util::Sum, Effect, EffectList,
        Effectful,
    };

    struct Eff1(u32);
    impl Effect for Eff1 {
        type Resume = u64;
    }

    struct Eff2(bool);
    impl Effect for Eff2 {
        type Resume = char;
    }

    struct Eff3(String);
    impl Effect for Eff3 {
        type Resume = u32;
    }

    fn a1() -> impl Effectful<EffectList![Eff1], Return = i16> {
        effectful_block! {
            #![effectful(Eff1)]
            (yield Eff1(1)) as i16
        }
    }

    struct Empty;

    impl Coroutine<Sum<(Begin, ())>> for Empty {
        type Yield = Sum<()>;
        type Return = ();

        fn resume(
            self: core::pin::Pin<&mut Self>,
            _: Sum<(Begin, ())>,
        ) -> core::ops::CoroutineState<Self::Yield, Self::Return> {
            core::ops::CoroutineState::Complete(())
        }
    }

    #[effectful(Eff1, Eff2)]
    fn b() -> i16 {
        Empty.await;

        let a2 = effectful_block! {
            #![effectful(Eff2)]
            (yield Eff2(true)) as i16
        };

        a1().await + a2.await
    }

    #[test]
    fn basic() {
        let coro = b();

        let coro = coro.handle(handler! {
            Eff1(100..) => 100,
            ref eff @ Eff1(_) => eff.0 as u64,
        });

        let coro = coro.catch0(handler! {
            #![effectful(Eff2)]
            eff @ Eff2(_) => yield eff,
        });

        let coro = coro.catch1(handler! {
            #![effectful(Eff3)]
            Eff2(c) => char::try_from(yield Eff3(c.to_string())).unwrap_or('d')
        });

        let ret = crate::catch!(static coro.await {
            Eff3(y) if y == "true" => 1,
            Eff3(_) => break 100,
        });

        assert_eq!(ret, 2);
    }

    #[crate::group]
    trait Counter {
        fn inc(delta: u32);

        fn get() -> u32;
    }

    struct CounterImpl(u32);

    #[crate::group_handler]
    impl Counter for CounterImpl {
        fn inc(&mut self, delta: u32) {
            self.0 += delta;
        }

        fn get(&self) -> u32 {
            self.0
        }
    }

    struct CounterAmplifier(u32);

    #[crate::group_handler]
    #[effectful(Counter)]
    impl Counter for CounterAmplifier {
        fn inc(&self, delta: u32) {
            Counter::inc(delta * self.0).await
        }

        fn get(&self) -> u32 {
            Counter::get().await / self.0
        }
    }

    #[test]
    fn group() {
        let coro = effectful_block! {
            #![effectful(Counter)]

            let counter = Counter::get().await;
            if counter < 10 {
                Counter::inc(10 - counter).await;
            }
            Counter::get().await
        };

        let amp = CounterAmplifier(10);
        let coro = coro.catch0(amp);

        let mut counter = CounterImpl(0);
        let coro = coro.handle(&mut counter);

        assert_eq!(coro.run(), 10);
        assert_eq!(counter.get::<()>(), core::ops::ControlFlow::Continue(100));
    }
}
