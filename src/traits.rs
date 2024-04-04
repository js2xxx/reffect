use core::{
    convert::Infallible,
    fmt::Debug,
    ops::{Coroutine, Deref, DerefMut},
};

use tuple_list::{Tuple, TupleList};

use crate::util::{sum_type::repr::TupleSum, Sum};

pub trait Effect {
    type Resume;

    fn tag(r: Self::Resume) -> ResumeTy<Self> {
        ResumeTy(r)
    }

    fn untag(rt: ResumeTy<Self>) -> Self::Resume {
        rt.0
    }
}

#[macro_export]
macro_rules! Effects {
    [$($t:ty),* $(,)?] => {
        $crate::Sum![$($t,)*]
    };
}

#[macro_export]
macro_rules! Resumes {
    ($($t:ty),* $(,)?) => {
        $crate::Sum![$($crate::ResumeTy<$t>,)*]
    };
}

pub trait EffectList: TupleList {
    type ResumeList: TupleList;
}

impl EffectList for () {
    type ResumeList = ();
}

impl<T: Effect, U: EffectList> EffectList for (T, U)
where
    (T, U): TupleList,
    (ResumeTy<T>, U::ResumeList): TupleList,
{
    type ResumeList = (ResumeTy<T>, U::ResumeList);
}

pub type ResumeTuple<E> = <<<E as Tuple>::TupleList as EffectList>::ResumeList as TupleList>::Tuple;

#[derive(Clone, Copy)]
pub struct ResumeTy<E: Effect + ?Sized>(E::Resume);

impl<E: Effect + ?Sized> Debug for ResumeTy<E>
where
    E::Resume: Debug,
{
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "{:?} (of {})", self.0, core::any::type_name::<E>())
    }
}

impl<E: Effect + ?Sized> ResumeTy<E> {
    pub fn tag(r: E::Resume) -> Self {
        Self(r)
    }

    pub fn untag(self) -> E::Resume {
        self.0
    }
}

impl<E: Effect + ?Sized> Deref for ResumeTy<E> {
    type Target = E::Resume;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<E: Effect + ?Sized> DerefMut for ResumeTy<E> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub trait Effectful<E: TupleSum = (Infallible,)>:
    Coroutine<Sum<ResumeTuple<E>>, Yield = Sum<E>>
where
    E::TupleList: EffectList,
    ResumeTuple<E>: TupleSum,
{
}

impl<Coro: Coroutine<Sum<ResumeTuple<E>>, Yield = Sum<E>>, E: TupleSum> Effectful<E> for Coro
where
    E::TupleList: EffectList,
    ResumeTuple<E>: TupleSum,
{
}

#[diagnostic::on_unimplemented(
    message = "`{Self}` is not a coroutine",
    label = "`{Self}` is not a coroutine",
    note = "{Self} must be a coroutine or must implement `IntoCoroutine` to be resumed"
)]
pub trait IntoCoroutine<Marker, R = ()> {
    type Yield;
    type Return;
    type IntoCoroutine: Coroutine<R, Yield = Self::Yield, Return = Self::Return>;

    fn into_coroutine(self) -> Self::IntoCoroutine;
}

impl<Coro, R, Y, T> IntoCoroutine<(), R> for Coro
where
    Coro: Coroutine<R, Yield = Y, Return = T>,
{
    type Yield = Y;
    type Return = T;
    type IntoCoroutine = Coro;

    fn into_coroutine(self) -> Self::IntoCoroutine {
        self
    }
}
