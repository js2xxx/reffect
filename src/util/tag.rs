use core::marker::PhantomData;

pub struct UTerm;

pub struct UInt<U>(PhantomData<U>);

pub trait Tag {
    const VALUE: u8;
}

impl Tag for UTerm {
    const VALUE: u8 = 0;
}

impl<U: Tag> Tag for UInt<U> {
    const VALUE: u8 = 1 + U::VALUE;
}

pub type U0 = UTerm;
pub type U1 = UInt<U0>;
pub type U2 = UInt<U1>;
pub type U3 = UInt<U2>;
pub type U4 = UInt<U3>;
pub type U5 = UInt<U4>;
pub type U6 = UInt<U5>;
pub type U7 = UInt<U6>;
pub type U8 = UInt<U7>;
pub type U9 = UInt<U8>;

pub type U10 = UInt<U9>;
pub type U11 = UInt<U10>;
pub type U12 = UInt<U11>;

pub trait Bool {}

pub struct True;

pub struct False;

impl Bool for True {}

impl Bool for False {}

pub trait UCmp<U> {
    const CMP: core::cmp::Ordering;

    type Ge: Bool;
    type Le: Bool;
}

impl UCmp<UTerm> for UTerm {
    const CMP: core::cmp::Ordering = core::cmp::Ordering::Equal;

    type Ge = True;
    type Le = True;
}

impl UCmp<UTerm> for UInt<UTerm> {
    const CMP: core::cmp::Ordering = core::cmp::Ordering::Greater;

    type Ge = True;
    type Le = False;
}

impl UCmp<UInt<UTerm>> for UTerm {
    const CMP: core::cmp::Ordering = core::cmp::Ordering::Less;

    type Ge = False;
    type Le = True;
}

impl<U, V> UCmp<UInt<V>> for UInt<U>
where
    U: UCmp<V>,
{
    const CMP: core::cmp::Ordering = U::CMP;

    type Ge = U::Ge;
    type Le = U::Le;
}

pub trait Ordered {}

impl Ordered for () {}

impl<T> Ordered for (T, ()) {}

impl<Head, Next, Tail> Ordered for (Head, (Next, Tail))
where
    Head: UCmp<Next, Ge = True>,
    (Next, Tail): Ordered,
{
}
