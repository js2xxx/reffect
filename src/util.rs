pub mod sum_type;
pub mod tag;

use core::{any::type_name, marker::PhantomData};

pub use self::sum_type::{range::ConcatList, Sum};
use self::sum_type::{range::SplitList, repr::SumList};
use crate::{
    adapter::Begin,
    effect::{EffectList, ResumeTy},
    Effect,
};

pub fn mark<T: ?Sized>(_: &T) -> PhantomData<T> {
    PhantomData
}

pub fn type_name_of_marker<T: ?Sized>(_: PhantomData<T>) -> &'static str {
    type_name::<T>()
}

pub fn resume_marker<E: EffectList>(_: PhantomData<E>) -> PhantomData<(Begin, E::ResumeList)> {
    PhantomData
}

pub fn untag_effect<T: Effect>(r: Sum<(ResumeTy<T>, ())>, _: PhantomData<(T, ())>) -> T::Resume {
    Sum::into_inner(r).untag()
}

fn narrow_effect_impl<R, RR, E, UL>(r: Sum<R>, marker: PhantomData<E>) -> Sum<RR>
where
    RR: SumList,
    E: EffectList,
    R: SplitList<RR, UL>,
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

pub fn narrow_effect<R, E, UL>(r: Sum<R>, marker: PhantomData<E>) -> Sum<E::ResumeList>
where
    E: EffectList,
    R: SplitList<E::ResumeList, UL>,
{
    narrow_effect_impl(r, marker)
}

pub fn narrow_effect_prefixed<R, E, UL>(
    r: Sum<R>,
    marker: PhantomData<E>,
) -> Sum<(Begin, E::ResumeList)>
where
    E: EffectList,
    R: SplitList<(Begin, E::ResumeList), UL>,
{
    narrow_effect_impl(r, marker)
}
