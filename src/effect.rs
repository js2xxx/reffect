use core::{
    fmt::Debug,
    ops::{Coroutine, CoroutineState, Deref, DerefMut},
    pin::Pin,
};

use crate::{
    adapter::Begin,
    util::{sum_type::repr::SumList, Sum},
};

pub trait Effect {
    type Resume;
}

pub trait EffectExt: Effect {
    fn tag(r: Self::Resume) -> ResumeTy<Self> {
        ResumeTy(r)
    }

    fn untag(rt: ResumeTy<Self>) -> Self::Resume {
        rt.0
    }
}

impl<E: Effect> EffectExt for E {}

pub trait EffectGroup {
    type Effects;
}

impl<E: Effect> EffectGroup for E {
    type Effects = (E, ());
}

pub trait EffectList: SumList {
    type ResumeList: SumList;
}

impl EffectList for () {
    type ResumeList = ();
}

impl<T: Effect, U: EffectList> EffectList for (T, U)
where
    (T, U): SumList,
    (ResumeTy<T>, U::ResumeList): SumList,
{
    type ResumeList = (ResumeTy<T>, U::ResumeList);
}

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

pub trait Effectful<E: EffectList = ()>:
    Coroutine<Sum<(Begin, E::ResumeList)>, Yield = Sum<E>>
{
}

impl<Coro, E> Effectful<E> for Coro
where
    Coro: Coroutine<Sum<(Begin, E::ResumeList)>, Yield = Sum<E>>,
    E: EffectList,
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

pub struct FuncCoro<F>(Option<F>);

impl<F> Unpin for FuncCoro<F> {}

impl<F, T> Coroutine<Sum<(Begin, ())>> for FuncCoro<F>
where
    F: FnOnce() -> T,
{
    type Yield = Sum<()>;
    type Return = T;

    fn resume(mut self: Pin<&mut Self>, _: Sum<(Begin, ())>) -> CoroutineState<Sum<()>, T> {
        match self.0.take() {
            Some(f) => CoroutineState::Complete(f()),
            None => panic!("function coroutine resumed after completion"),
        }
    }
}

pub enum FuncMarker {}
impl<F, T> IntoCoroutine<FuncMarker, Sum<(Begin, ())>> for F
where
    F: FnOnce() -> T,
{
    type Yield = Sum<()>;
    type Return = T;
    type IntoCoroutine = FuncCoro<F>;

    fn into_coroutine(self) -> Self::IntoCoroutine {
        FuncCoro(Some(self))
    }
}
