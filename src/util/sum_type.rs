mod derive;
pub mod range;
pub mod repr;

use core::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    mem::{self, ManuallyDrop, MaybeUninit},
    ops::{Deref, DerefMut},
    ptr,
};

pub use tuple_list::tuple_list_type as umap;

use super::tag::{Tag, UTerm};

pub type Repr<S> = <S as repr::SumList>::Repr;

#[macro_export]
macro_rules! Sum {
    (@FORWARD) => [()];
    (@FORWARD $head:ty, $($t:ty,)*) => [($head, $crate::Sum!(@FORWARD $($t,)*))];
    [$($t:ty),* $(,)?] => {
        $crate::util::Sum::<$crate::Sum!(@FORWARD $($t,)*)>
    };
}

pub struct Sum<S: repr::SumList> {
    tag: u8,
    data: ManuallyDrop<Repr<S>>,
}

impl<T> From<T> for Sum![T] {
    fn from(value: T) -> Self {
        Sum::new(value)
    }
}

impl<T> Deref for Sum![T] {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*<(T, ()) as repr::Split<T, UTerm>>::as_ptr(&self.data) }
    }
}

impl<T> DerefMut for Sum![T] {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *<(T, ()) as repr::Split<T, UTerm>>::as_mut_ptr(&mut self.data) }
    }
}

impl<T> Sum![T] {
    pub fn into_inner(self) -> T {
        unsafe {
            let this = ManuallyDrop::new(self);
            mem::transmute_copy(&this.data)
        }
    }
}

impl Sum![] {
    pub fn unreachable(self) -> ! {
        match self.data.0 {}
    }
}

impl<S: repr::SumList> Sum<S> {
    pub fn type_id(&self) -> core::any::TypeId
    where
        S: derive::TypeMeta + 'static,
    {
        S::type_id(self.tag)
    }

    pub fn type_name(&self) -> &'static str
    where
        S: derive::TypeMeta,
    {
        S::type_name(self.tag)
    }

    pub fn as_any(&self) -> &dyn core::any::Any
    where
        S: derive::TypeMeta + 'static,
    {
        unsafe { S::as_any(&self.data, self.tag) }
    }

    pub fn as_any_mut(&mut self) -> &mut dyn core::any::Any
    where
        S: derive::TypeMeta + 'static,
    {
        unsafe { S::as_any_mut(&mut self.data, self.tag) }
    }
}

impl<S: repr::SumList> Sum<S> {
    pub fn new<T, U>(value: T) -> Self
    where
        S: repr::Split<T, U>,
        U: Tag,
    {
        Sum {
            tag: U::VALUE,
            data: ManuallyDrop::new(S::from_data(value)),
        }
    }

    pub fn new_marked<T, U>(value: T, _: PhantomData<S>) -> Self
    where
        S: repr::Split<T, U>,
        U: Tag,
    {
        Self::new(value)
    }

    pub fn type_marker(&self) -> PhantomData<S> {
        PhantomData
    }

    pub fn get<T, U>(&self) -> Option<&T>
    where
        S: repr::Split<T, U>,
        U: Tag,
    {
        (self.tag == U::VALUE).then(|| unsafe { &*S::as_ptr(&self.data) })
    }

    pub fn get_mut<T, U>(&mut self) -> Option<&mut T>
    where
        S: repr::Split<T, U>,
        U: Tag,
    {
        (self.tag == U::VALUE).then(|| unsafe { &mut *S::as_mut_ptr(&mut self.data) })
    }
}

pub type Rem<S, T, U> = <S as repr::Split<T, U>>::Remainder;
pub type RemTags<S, T, U> = <S as repr::Split<T, U>>::RemainderTags;
pub type Substitute<S, T, T2, U> = <S as repr::Split<T, U>>::Substitute<T2>;

impl<S: repr::SumList> Sum<S> {
    pub fn try_unwrap<T, U>(self) -> Result<T, Sum<Rem<S, T, U>>>
    where
        S: repr::Split<T, U>,
        U: Tag,
    {
        let mut this = ManuallyDrop::new(self);
        match S::try_unwrap(this.tag) {
            Ok(()) => Ok(unsafe { S::into_data_unchecked(ManuallyDrop::take(&mut this.data)) }),
            Err(tag) => unsafe {
                let data = mem::transmute_copy(&this.data);
                Err(Sum { tag, data })
            },
        }
    }

    pub fn map<T, T2, U>(self, f: impl FnOnce(T) -> T2) -> Sum<Substitute<S, T, T2, U>>
    where
        S: repr::Split<T, U>,
        U: Tag,
    {
        let mut this = ManuallyDrop::new(self);
        match S::try_unwrap(this.tag) {
            Ok(()) => {
                let data = f(unsafe { S::into_data_unchecked(ManuallyDrop::take(&mut this.data)) });
                Sum {
                    tag: this.tag,
                    data: ManuallyDrop::new(
                        <Substitute<S, T, T2, U> as repr::Split<T2, U>>::from_data(data),
                    ),
                }
            }
            Err(_) => unsafe {
                let data = mem::transmute_copy(&this.data);
                Sum { tag: this.tag, data }
            },
        }
    }
}

pub type NarrowRem<S, S2, UMap> = <S as range::SplitList<S2, UMap>>::Remainder;

impl<S: repr::SumList> Sum<S> {
    pub fn narrow<S2, UMap>(self) -> Result<Sum<S2>, Sum<NarrowRem<S, S2, UMap>>>
    where
        S: range::SplitList<S2, UMap>,
        S2: repr::SumList,
    {
        let this = ManuallyDrop::new(self);
        unsafe {
            match <S as range::SplitList<S2, UMap>>::narrow_tag(this.tag) {
                Ok(tag) => {
                    let data = mem::transmute_copy(&this.data);
                    Ok(Sum { tag, data })
                }
                Err(tag) => {
                    let data = mem::transmute_copy(&this.data);
                    Err(Sum { tag, data })
                }
            }
        }
    }

    pub fn broaden<S2, UMap>(self) -> Sum<S2>
    where
        S2: range::SplitList<S, UMap>,
    {
        unsafe {
            let tag = <S2 as range::SplitList<S, UMap>>::broaden_tag(self.tag);
            let mut data = MaybeUninit::<ManuallyDrop<S2::Repr>>::uninit();
            data.as_mut_ptr()
                .cast::<ManuallyDrop<S::Repr>>()
                .write(ptr::read(&self.data));
            let data = data.assume_init();
            mem::forget(self);
            Sum { tag, data }
        }
    }
}

impl<S: derive::SumDebug> fmt::Debug for Sum<S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", unsafe { S::debug(&self.data, self.tag) })
    }
}

impl<S: repr::SumList> Drop for Sum<S> {
    fn drop(&mut self) {
        unsafe { S::drop(&mut self.data, self.tag) }
    }
}

impl<S: derive::SumClone> Clone for Sum<S> {
    fn clone(&self) -> Self {
        Sum {
            tag: self.tag,
            data: unsafe { S::clone(&self.data, self.tag) },
        }
    }
}

impl<S: derive::SumPartialEq> PartialEq for Sum<S> {
    fn eq(&self, other: &Self) -> bool {
        self.tag == other.tag && unsafe { S::eq(&self.data, &other.data, self.tag) }
    }
}

impl<S: derive::SumPartialEq + Eq> Eq for Sum<S> {}

impl<S: derive::SumPartialOrd> PartialOrd for Sum<S> {
    fn partial_cmp(&self, other: &Self) -> Option<core::cmp::Ordering> {
        match self.tag.cmp(&other.tag) {
            core::cmp::Ordering::Equal => unsafe {
                S::partial_cmp(&self.data, &other.data, self.tag)
            },
            other => Some(other),
        }
    }
}

impl<S: derive::SumOrd + Eq> Ord for Sum<S> {
    fn cmp(&self, other: &Self) -> core::cmp::Ordering {
        self.tag
            .cmp(&other.tag)
            .then_with(|| unsafe { S::cmp(&self.data, &other.data, self.tag) })
    }
}

impl<S: derive::SumHash> Hash for Sum<S> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.tag.hash(state);
        unsafe { S::hash(&self.data, self.tag, state) }
    }
}

#[cfg(test)]
mod tests {
    use std::string::{String, ToString};

    use super::*;
    use crate::util::tag::*;

    #[test]
    fn basic() {
        type T0 = (u32, ());
        type T1 = (u32, (String, ()));
        type T2 = (u32, (String, (u32, ())));

        let sum: Sum<T0> = 12345.into();
        assert_eq!(sum.get(), Some(&12345));

        let mut sum: Sum<T1> = sum.broaden();
        assert_eq!(sum.get::<u32, _>(), Some(&12345));
        assert_eq!(sum.get::<_, U1>(), None);

        sum = Sum::new("Hello World!".to_string());
        assert_eq!(sum.get(), Some(&"Hello World!".to_string()));

        let sum: Sum<T2> = sum.broaden::<_, umap![U2, U1]>();
        assert_eq!(sum.get(), Some(&"Hello World!".to_string()));

        let sum: Sum<T1> = sum.narrow::<_, umap![U0, U0]>().unwrap();
        let sum: Sum<(String, ())> = sum.narrow::<T0, _>().unwrap_err();
        assert_eq!(*sum, "Hello World!");
    }
}
