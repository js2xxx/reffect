#![no_std]
#![deny(future_incompatible)]
#![deny(rust_2018_idioms)]
#![deny(rust_2024_compatibility)]
#![feature(coroutines)]
#![feature(coroutine_trait)]
#![feature(macro_metavar_expr)]
#![feature(noop_waker)]

pub mod adapter;
pub mod future;
mod traits;
pub mod util;

#[cfg(test)]
extern crate std;

pub use self::traits::*;
pub use reffect_macros::{effectful_block, effectful};