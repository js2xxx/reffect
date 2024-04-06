mod catch;
mod handle;
mod transform;

use core::{
    ops::{Coroutine, CoroutineState::*},
    pin::pin,
};

pub use self::{
    catch::{catch, Catch},
    handle::{handle, Handle},
    transform::{transform, transform0, transform1, Transform, Transform0, Transform1},
};
use crate::{
    effect::{EffectList, Effectful},
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

    fn catch<Trans, H, MTypes, MULists>(
        self,
        trans: Trans,
    ) -> Catch<Self, Trans, H, MTypes, MULists> {
        catch(self, trans)
    }

    fn transform<Trans, H, MTypes, MULists>(
        self,
        trans: Trans,
    ) -> Transform<Self, Trans, H, MTypes, MULists> {
        transform(self, trans)
    }

    fn transform0<Trans, E, H, HY, EUL, RemEUL, HUL>(
        self,
        trans: Trans,
    ) -> Transform0<Self, Trans, H, Y, E, HY, EUL, RemEUL, HUL>
    where
        Trans: FnMut(Sum<E>) -> H,
        H: Effectful<HY, Return = Sum<E::ResumeList>>,

        E: EffectList,
        HY: EffectList,

        Y: EffectList + ContainsList<E, EUL, RemEUL> + SplitList<HY, HUL>,
        Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL> + SplitList<HY::ResumeList, HUL>,
    {
        transform(self, trans)
    }

    fn transform1<Trans, E, H, HY, EUL, RemEUL>(
        self,
        trans: Trans,
    ) -> Transform1<Self, Trans, H, Y, E, HY, EUL, RemEUL>
    where
        Trans: FnMut(Sum<E>) -> H,
        H: Effectful<HY, Return = Sum<E::ResumeList>>,

        E: EffectList,
        HY: EffectList + ConcatList<NarrowRem<Y, E, EUL>>,
        HY::ResumeList: ConcatList<NarrowRem<Y::ResumeList, E::ResumeList, EUL>>,

        Y: EffectList + ContainsList<E, EUL, RemEUL>,
        Y::ResumeList: ContainsList<E::ResumeList, EUL, RemEUL>,
    {
        transform(self, trans)
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
        self as reffect,
        effect::{EffectExt, ResumeTy},
        effectful, effectful_block, handler,
        util::Sum,
        Effect, EffectList, Effectful, Effects, Resumes,
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

        let coro = coro.transform0(|eff: Effects![Eff2]| {
            move |_: Resumes![Eff2]| {
                let sum = yield eff.broaden::<EffectList![Eff2], _>();
                sum.narrow::<(ResumeTy<Eff2>, ()), _>().unwrap()
            }
        });

        let coro = coro.transform1(|eff: Effects![Eff2]| {
            move |_: Resumes![Eff3]| {
                let r = yield eff
                    .map(|Eff2(c)| Eff3(c.to_string()))
                    .broaden::<(Eff3, ()), _>();
                r.narrow::<(ResumeTy<Eff3>, ()), _>()
                    .unwrap()
                    .map(|i: ResumeTy<Eff3>| Eff2::tag(char::try_from(i.untag()).unwrap_or('d')))
            }
        });

        let coro = coro.handle(handler! {
            Eff3(y) if y == "true" => 1,
            Eff3(_) => break 2,
        });

        assert_eq!(coro.run(), 2);
    }
}
