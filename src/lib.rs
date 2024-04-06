#![doc = include_str!("../README.md")]
#![no_std]
#![deny(future_incompatible)]
#![deny(rust_2018_idioms)]
#![deny(rust_2024_compatibility)]
#![feature(coroutines)]
#![feature(coroutine_trait)]
#![feature(macro_metavar_expr)]
#![feature(noop_waker)]

pub mod adapter;
pub mod effect;
pub mod future;
pub mod util;

#[cfg(test)]
extern crate std;

pub use reffect_macros::{do_await, do_yield, effectful, effectful_block, handler, EffectList};

pub use self::effect::{Effect, EffectExt, EffectGroup, Effectful};
