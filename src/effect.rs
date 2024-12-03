use core::{
    fmt::Debug,
    ops::{ControlFlow, Coroutine, CoroutineState, Deref, DerefMut},
    pin::Pin,
};

use crate::{
    adapter::Begin,
    util::{
        Sum,
        sum_type::{range::SplitList, repr::SumList},
        tag::UTerm,
    },
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

pub trait EffectList: Sized + SumList + SplitList<Self, <Self as SumList>::Tags<UTerm>> {
    type ResumeList: SumList
        + SplitList<Self::ResumeList, <Self::ResumeList as SumList>::Tags<UTerm>>;
}

impl EffectList for () {
    type ResumeList = ();
}

impl<T: Effect, U: EffectList> EffectList for (T, U)
where
    (T, U): SumList + SplitList<(T, U), <(T, U) as SumList>::Tags<UTerm>>,
    (ResumeTy<T>, U::ResumeList): SumList
        + SplitList<
            (ResumeTy<T>, U::ResumeList),
            <(ResumeTy<T>, U::ResumeList) as SumList>::Tags<UTerm>,
        >,
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

pub trait Catcher<T, E: EffectList, F: EffectList = (), Marker = ()> {
    type Catcher<'a>: Effectful<F, Return = ControlFlow<T, Sum<E::ResumeList>>>
    where
        Self: 'a;

    fn catch(self: Pin<&mut Self>, effect: Sum<E>) -> Self::Catcher<'_>;
}

impl<H, T, E, F> Catcher<T, E, F> for &mut H
where
    H: Catcher<T, E, F> + Unpin,
    E: EffectList,
    F: EffectList,
{
    type Catcher<'a>
        = H::Catcher<'a>
    where
        Self: 'a;

    fn catch(self: Pin<&mut Self>, effect: Sum<E>) -> Self::Catcher<'_> {
        H::catch(Pin::new(*self.get_mut()), effect)
    }
}

impl<H, T, E, F> Catcher<T, E, F> for Pin<&mut H>
where
    H: Catcher<T, E, F>,
    E: EffectList,
    F: EffectList,
{
    type Catcher<'a>
        = H::Catcher<'a>
    where
        Self: 'a;

    fn catch(self: Pin<&mut Self>, effect: Sum<E>) -> Self::Catcher<'_> {
        H::catch(self.get_mut().as_mut(), effect)
    }
}

pub trait Handler<T, E: EffectList, Marker = ()> {
    fn handle(self: Pin<&mut Self>, effect: Sum<E>) -> ControlFlow<T, Sum<E::ResumeList>>;
}

impl<H, T, E> Handler<T, E> for &mut H
where
    H: Handler<T, E> + Unpin,
    E: EffectList,
{
    fn handle(self: Pin<&mut Self>, effect: Sum<E>) -> ControlFlow<T, Sum<E::ResumeList>> {
        H::handle(Pin::new(*self.get_mut()), effect)
    }
}

impl<H, T, E> Handler<T, E> for Pin<&mut H>
where
    H: Handler<T, E>,
    E: EffectList,
{
    fn handle(self: Pin<&mut Self>, effect: Sum<E>) -> ControlFlow<T, Sum<E::ResumeList>> {
        H::handle(self.get_mut().as_mut(), effect)
    }
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

impl<C, H, T, E, F> Catcher<T, E, F, FuncMarker> for C
where
    C: FnMut(Sum<E>) -> H + Unpin,
    H: Effectful<F, Return = ControlFlow<T, Sum<E::ResumeList>>>,
    E: EffectList,
    F: EffectList,
{
    type Catcher<'a>
        = H
    where
        C: 'a;

    fn catch(mut self: Pin<&mut Self>, effect: Sum<E>) -> Self::Catcher<'_> {
        self(effect)
    }
}

impl<H, T, E> Handler<T, E, FuncMarker> for H
where
    H: FnMut(Sum<E>) -> ControlFlow<T, Sum<E::ResumeList>> + Unpin,
    E: EffectList,
{
    fn handle(mut self: Pin<&mut Self>, effect: Sum<E>) -> ControlFlow<T, Sum<E::ResumeList>> {
        self(effect)
    }
}
