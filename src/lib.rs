#![doc = include_str!("../README.md")]
#![no_std]
#![feature(coroutines)]
#![feature(coroutine_trait)]
#![cfg_attr(test, feature(noop_waker))]

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
