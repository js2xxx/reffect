#![doc = include_str!("../README.md")]
#![no_std]
#![deny(future_incompatible)]
#![deny(rust_2018_idioms)]
#![deny(rust_2024_compatibility)]
#![feature(coroutines)]
#![feature(coroutine_trait)]
#![feature(impl_trait_in_assoc_type)]
#![feature(lifetime_capture_rules_2024)]
#![feature(macro_metavar_expr)]
#![feature(noop_waker)]

pub mod adapter;
pub mod effect;
pub mod future;
pub mod util;

#[cfg(test)]
extern crate std;

pub use reffect_macros::{
    EffectList, catch, do_await, do_yield, effectful, effectful_block, group, group_handler,
    handler,
};

pub use self::{
    adapter::EffectfulExt,
    effect::{Effect, EffectExt, EffectGroup, Effectful},
};
