mod catch;
mod handle;
mod transform;

use core::{
    convert::Infallible,
    ops::{Coroutine, CoroutineState::*},
    pin::pin,
};

use tuple_list::Tuple;

pub use self::{
    catch::{catch, Catch},
    handle::{handle, Handle},
    transform::{transform, transform0, transform1, Transform, Transform0},
};
use crate::{
    util::{sum_type::repr::TupleSum, Sum},
    Effect, EffectList, Effectful, ResumeTuple, ResumeTy,
};

type ResumeList<T> = <<T as Tuple>::TupleList as EffectList>::ResumeList;

#[derive(Debug)]
pub struct Begin;

impl Effect for Infallible {
    type Resume = Begin;
}

pub fn run<Coro: Effectful>(coro: Coro) -> Coro::Return {
    match pin!(coro).resume(Sum::new(ResumeTy::tag(Begin))) {
        Complete(ret) => ret,
        Yielded(eff) => eff.unreachable(),
    }
}

pub trait EffectfulExt<E: TupleSum>: Effectful<E> + Sized
where
    E::TupleList: EffectList,
    ResumeTuple<E>: TupleSum,
{
    fn run(self) -> <Self as Coroutine<Sum<ResumeTuple<(Infallible,)>>>>::Return
    where
        Self: Effectful + Coroutine<Sum<ResumeTuple<(Infallible,)>>>,
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
    E: TupleSum,
    E::TupleList: EffectList,
    ResumeTuple<E>: TupleSum,
    F: Effectful<E>,
{
}

#[cfg(test)]
mod test {
    use core::{
        convert::Infallible,
        ops::{ControlFlow::*, Coroutine},
    };
    use std::string::{String, ToString};

    use super::{run, transform0, transform1, EffectfulExt};
    use crate::{
        self as reffect, effectful, effectful_block, util::Sum, Effect, Effectful, Effects,
        ResumeTy, Resumes,
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

    #[effectful(Infallible, Eff1)]
    fn a1() -> i16 {
        (yield Eff1(1)) as i16
    }

    fn a2() -> impl Effectful<(Infallible, Eff2), Return = i16> {
        effectful_block! {
            #![effectful(Infallible, Eff2)]
            (yield Eff2(true)) as i16
        }
    }

    struct Empty;

    impl Coroutine<Sum<(ResumeTy<Infallible>,)>> for Empty {
        type Yield = Sum<(Infallible,)>;
        type Return = ();

        fn resume(
            self: core::pin::Pin<&mut Self>,
            _: Sum<(ResumeTy<Infallible>,)>,
        ) -> core::ops::CoroutineState<Self::Yield, Self::Return> {
            core::ops::CoroutineState::Complete(())
        }
    }

    #[effectful(Infallible, Eff1, Eff2)]
    fn b() -> i16 {
        Empty.await;
        a1().await + a2().await
    }

    fn assert_send<T: Send>(_: &T) {}

    #[test]
    fn basic() {
        let coro = b();
        assert_send(&coro);
        let coro =
            coro.handle(|x: Sum<(Eff1,)>| Continue(Eff1::tag(x.into_inner().0 as u64).into()));
        let coro = transform0(coro, |eff: Effects![Infallible, Eff2]| {
            move |_: Resumes![Infallible, Eff2]| yield eff
        });
        let coro = transform1(coro, |eff: Effects![Infallible, Eff2]| {
            move |_: Resumes![Infallible, Eff3]| {
                let r = yield eff.map(|Eff2(c)| Eff3(c.to_string()));
                r.map(|i: ResumeTy<Eff3>| Eff2::tag(char::try_from(i.untag()).unwrap_or('d')))
            }
        });
        let coro = coro.handle(|y: Sum<(Eff3,)>| {
            Continue(Eff3::tag(if y.into_inner().0 == "true" { 1 } else { 2 }).into())
        });

        assert_eq!(run(coro), 2);
    }
}
