pub mod sum_type;
pub mod tag;
pub(crate) mod tuple;

use core::{any::type_name, marker::PhantomData};

use tuple_list::TupleList;

use self::sum_type::{range::TupleRange, repr::TupleSum};
pub use self::sum_type::{umap, Sum};
use crate::{Effect, EffectList, ResumeTuple, ResumeTy};

pub fn mark<T: ?Sized>(_: &T) -> PhantomData<T> {
    PhantomData
}

pub fn type_name_of_marker<T: ?Sized>(_: PhantomData<T>) -> &'static str {
    type_name::<T>()
}

pub fn untag_effect<T: Effect>(r: Sum<(ResumeTy<T>,)>, _: PhantomData<(T,)>) -> T::Resume {
    Sum::into_inner(r).untag()
}

pub fn narrow_effect<R, E, UL>(r: Sum<R>, marker: PhantomData<E>) -> Sum<ResumeTuple<E>>
where
    ResumeTuple<E>: TupleSum,

    E: TupleSum,
    E::TupleList: EffectList<Tuple = E>,

    R: TupleSum,
    R::TupleList: TupleList<Tuple = R>,

    R::TupleList: TupleRange<<E::TupleList as EffectList>::ResumeList, UL>,
{
    match r.narrow() {
        Ok(state) => state,
        Err(e) => panic!(
            "resumed with {}, which is not handled by effect {}",
            core::any::type_name_of_val(&e),
            crate::util::type_name_of_marker(marker),
        ),
    }
}

#[macro_export]
macro_rules! do_yield {
    ($eff:expr) => {{
        let eff = $crate::util::Sum::from($eff);
        let marker = eff.type_marker();
        let r = $crate::util::narrow_effect(yield eff.broaden(), marker);
        $crate::util::untag_effect(r, marker)
    }};
}

#[macro_export]
macro_rules! do_await {
    ($e:expr) => {{
        use core::ops::{Coroutine, CoroutineState::*};
        let mut coro = $crate::traits::IntoCoroutine::into_coroutine($e);

        let mut state = $crate::util::Sum::new(<core::convert::Infallible as $crate::Effect>::tag(
            $crate::adapter::Begin,
        ));
        loop {
            // SAFETY: `coro` won't be moved.
            let eff = match unsafe { core::pin::Pin::new_unchecked(&mut coro) }.resume(state) {
                Yielded(eff) => eff,
                Complete(ret) => break ret,
            };
            let eff_marker = eff.type_marker();
            state = $crate::util::narrow_effect(yield eff.broaden(), eff_marker);
        }
    }};
}
