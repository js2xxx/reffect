# Yet another Rust algebraic effects library

## Examples

```rust
#![feature(coroutines)]

use std::convert::Infallible;
use std::ops::ControlFlow::*;
use reffect::{*, adapter::EffectfulExt, util::Sum};

struct Log(String);
impl Effect for Log {
    type Resume = ();
}

struct Increment(u32);
impl Effect for Increment {
    type Resume = u32;
}

#[effectful(Log)]
fn log_value<T: std::fmt::Debug>(value: T) -> T {
    yield Log(format!("{:?}", value));
    value
}

#[effectful(Increment)]
fn increment(value: u32) -> u32 {
    yield Increment(value)
}

#[effectful(Log, Increment)]
fn test_func() -> u32 {
    let value = log_value(1).await;
    let value = increment(value).await;
    log_value(value).await;
    value
}

let ret = test_func().handle(handler! {
    Log(s) => println!("{s}"),
    Increment(i) if i < 10 => i + 1,
    Increment(i) => break i,
});

assert_eq!(ret.run(), 2);

```

## TODO

- [x] `handler!(Eff1(x) | Eff2(x) => todo!())`
  - [x] Expand root `continue` to `ControlFlow::Continue`
  - [x] Effectful handlers (`transform`)
- [ ] `#[group]` on trait definition
- [ ] `#[group_handler]` on trait implementation
  - [ ] Effectful group handlers
- [ ] `catch!(expr.await { Eff1(x) => todo!() })` (In-place catch)