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
    transform::{transform, transform0, transform1, Transform, Transform0},
};
use crate::{
    util::{sum_type::repr::TupleSum, Sum},
    EffectList, Effectful, PrefixedResumeList, Sum,
};

#[derive(Debug)]
pub struct Begin;

pub fn run<Coro: Effectful>(coro: Coro) -> Coro::Return {
    match pin!(coro).resume(Sum::new(Begin)) {
        Complete(ret) => ret,
        Yielded(eff) => eff.unreachable(),
    }
}

pub trait EffectfulExt<E: EffectList>: Effectful<E> + Sized
where
    PrefixedResumeList<E>: TupleSum,
{
    fn run(self) -> <Self as Coroutine<Sum![Begin]>>::Return
    where
        Self: Effectful + Coroutine<Sum![Begin]>,
    {
        run(self)
    }

    fn handle<H, Markers>(self, handler: H) -> Handle<Self, H, Markers> {
        handle(self, handler)
    }

    fn transform<Trans, H, MTypes, MULists>(
        self,
        trans: Trans,
    ) -> Transform<Self, Trans, H, MTypes, MULists> {
        transform(self, trans)
    }
}

impl<F, E> EffectfulExt<E> for F
where
    E: EffectList,
    PrefixedResumeList<E>: TupleSum,
    F: Effectful<E>,
{
}

#[cfg(test)]
mod test {
    use core::ops::{ControlFlow::*, Coroutine};
    use std::string::{String, ToString};

    use super::{transform0, transform1, Begin, EffectfulExt};
    use crate::{
        self as reffect, effectful, effectful_block, util::Sum, Effect, Effectful, Effects, List,
        ResumeTy, Resumes, Sum,
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

    #[effectful(Eff1)]
    fn a1() -> i16 {
        (yield Eff1(1)) as i16
    }

    fn a2() -> impl Effectful<List![Eff2], Return = i16> {
        effectful_block! {
            #![effectful(Eff2)]
            (yield Eff2(true)) as i16
        }
    }

    struct Empty;

    impl Coroutine<Sum![Begin]> for Empty {
        type Yield = Sum<()>;
        type Return = ();

        fn resume(
            self: core::pin::Pin<&mut Self>,
            _: Sum![Begin],
        ) -> core::ops::CoroutineState<Self::Yield, Self::Return> {
            core::ops::CoroutineState::Complete(())
        }
    }

    #[effectful(Eff1, Eff2)]
    fn b() -> i16 {
        Empty.await;
        a1().await + a2().await
    }

    #[test]
    fn basic() {
        let coro = b();

        let coro = coro.handle(|x: Effects![Eff1]| {
            let r = x.into_inner().0 as u64;
            Continue(Eff1::tag(r).into())
        });

        let coro = transform0(coro, |eff: Effects![Eff2]| {
            move |_: Resumes![Eff2]| {
                let sum = yield eff.broaden::<List![Eff2], _>();
                sum.narrow::<(ResumeTy<Eff2>, ()), _>().unwrap()
            }
        });

        let coro = transform1(coro, |eff: Effects![Eff2]| {
            move |_: Resumes![Eff3]| {
                let r = yield eff
                    .map(|Eff2(c)| Eff3(c.to_string()))
                    .broaden::<(Eff3, ()), _>();
                r.narrow::<(ResumeTy<Eff3>, ()), _>()
                    .unwrap()
                    .map(|i: ResumeTy<Eff3>| Eff2::tag(char::try_from(i.untag()).unwrap_or('d')))
            }
        });

        let coro = coro.handle(|y: Effects![Eff3]| {
            let r = if y.into_inner().0 == "true" { 1 } else { 2 };
            Continue(Sum::from(Eff3::tag(r)))
        });

        assert_eq!(coro.run(), 2);
    }
}
