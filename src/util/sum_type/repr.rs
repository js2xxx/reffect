use tuple_list::Tuple;

use crate::util::tag::*;

pub trait TupleSum: Tuple {
    type Repr: Repr<Tuple = Self>;
}

pub trait Repr {
    type Tuple: TupleSum<Repr = Self>;

    type Tags: Tuple;
}

pub trait ReprMatch<T, U>: Repr {
    type Remainder: Repr;

    fn new(value: T) -> Self;

    fn from_remainder(remainder: Self::Remainder) -> Self;

    fn try_unwrap(self) -> Result<T, Self::Remainder>;

    fn get(&self) -> Option<&T>;

    fn get_mut(&mut self) -> Option<&mut T>;

    type Substitute<T2>: Repr;
    fn map<T2>(self, f: impl FnOnce(T) -> T2) -> Self::Substitute<T2>;
}

macro_rules! enums {
    ($name:ident: $($t:ident,)*) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum $name<$($t),*> {
            $($t($t),)*
        }

        impl<$($t),*> TupleSum for ($($t,)*) {
            type Repr = $name<$($t),*>;
        }

        impl<$($t),*> Repr for $name<$($t),*> {
            type Tuple = ($($t,)*);
            type Tags = ($(U0, ${ignore($t)})*);
        }
    };
    ($prev:ident => $name:ident: $($tag:ident,)*; $first:ident, $($t:ident,)*; $($u:ident,)*) => {
        enums!($name: $first, $($t,)*);

        enums!(@RECURSE $prev => $name:
            () $($tag,)*;
            () $first, $($t,)*; $($u,)*
        );
    };
    (@MATCH $prev:ident => $name:ident:
        ($($ts:ident,)*) $tag:ident, $($tt:ident,)*;
        ($($s:ident,)*) $first:ident, $($t:ident,)*; $($u:ident,)*
    ) => {
        impl<$($s,)* $first, $($t),*>
            ReprMatch<$first, $tag> for $name<$($s,)* $first, $($t),*>
        {
            type Remainder = $prev<$($s,)* $($t),*>;

            fn new(value: $first) -> Self {
                Self::$first(value)
            }

            fn from_remainder(remainder: $prev<$($s,)* $($t),*>) -> Self {
                match remainder {
                    $(
                        $prev::$s(value) => Self::$s(value),
                    )*
                    $(
                        $prev::$u(value) => Self::$t(value),
                    )*
                }
            }

            fn try_unwrap(self) -> Result<$first, Self::Remainder> {
                match self {
                    Self::$first(value) => Ok(value),
                    $(
                        Self::$s(value) => Err($prev::$s(value)),
                    )*
                    $(
                        Self::$t(value) => Err($prev::$u(value)),
                    )*
                }
            }

            #[allow(unreachable_patterns)]
            fn get(&self) -> Option<&$first> {
                match self {
                    Self::$first(value) => Some(value),
                    _ => None,
                }
            }

            #[allow(unreachable_patterns)]
            fn get_mut(&mut self) -> Option<&mut $first> {
                match self {
                    Self::$first(value) => Some(value),
                    _ => None,
                }
            }

            type Substitute<T2> = $name<$($s,)* T2, $($t,)*>;
            fn map<T2>(self, f: impl FnOnce($first) -> T2) -> Self::Substitute<T2> {
                match self {
                    $(
                        Self::$s(value) => $name::$s(value),
                    )*
                    Self::$first(value) => $name::$first(f(value)),
                    $(
                        Self::$t(value) => $name::$t(value),
                    )*
                }
            }
        }
    };
    (@RECURSE $prev:ident => $name:ident:
        ($($ts:ident,)*) $tag:ident,;
        ($($s:ident,)*) $first:ident,;
    ) => {
        enums!(@MATCH $prev => $name:
            ($($ts,)*) $tag,;
            ($($s,)*) $first,;
        );
    };
    (@RECURSE $prev:ident => $name:ident:
        ($($ts:ident,)*) $tag:ident, $tnext:ident, $($tt:ident,)*;
        ($($s:ident,)*) $first:ident, $next:ident, $($t:ident,)*; $unext:ident, $($u:ident,)*
    ) => {
        enums!(@MATCH $prev => $name:
            ($($ts,)*) $tag, $tnext, $($tt,)*;
            ($($s,)*) $first, $next, $($t,)*; $unext, $($u,)*
        );
        enums!(@RECURSE $prev => $name:
            ($($ts,)* $tag,) $tnext, $($tt,)*;
            ($($s,)* $first,) $next, $($t,)*; $($u,)*
        );
    };
}
enums!(E0:);
enums!(E0 => E1: U0,;A,;);
enums!(E1 => E2: U0, U1,;A, B,; A,);
enums!(E2 => E3: U0, U1, U2,;A, B, C,; A, B,);
enums!(E3 => E4: U0, U1, U2, U3,;A, B, C, D,; A, B, C,);
enums!(E4 => E5: U0, U1, U2, U3, U4,;A, B, C, D, E,; A, B, C, D,);
enums!(E5 => E6: U0, U1, U2, U3, U4, U5,; A, B, C, D, E, F,; A, B, C, D, E,);
enums!(E6 => E7: U0, U1, U2, U3, U4, U5, U6,; A, B, C, D, E, F, G,; A, B, C, D, E, F,);
enums!(E7 => E8: U0, U1, U2, U3, U4, U5, U6, U7,; A, B, C, D, E, F, G, H,; A, B, C, D, E, F, G,);
