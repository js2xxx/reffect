# Yet another Rust algebraic effects library

## Examples

### Standalone effects

```rust
#![feature(coroutines)]

use reffect::*;

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

### Grouped effects

```rust
#![feature(coroutines, coroutine_trait)]
#![feature(impl_trait_in_assoc_type)]
#![feature(lifetime_capture_rules_2024)]

use reffect::*;

#[group]
trait Counter {
    fn inc(delta: u32);

    fn get() -> u32;
}

struct CounterImpl(u32);

#[group_handler]
impl Counter for CounterImpl {
    fn inc(&mut self, delta: u32) {
        self.0 += delta;
    }

    fn get(&self) -> u32 {
        self.0
    }
}

struct CounterAmplifier(u32);

#[group_handler]
#[effectful(Counter)]
impl Counter for CounterAmplifier {
    fn inc(&self, delta: u32) {
        Counter::inc(delta * self.0).await
    }

    fn get(&self) -> u32 {
        Counter::get().await / self.0
    }
}

let coro = effectful_block! {
    #![effectful(Counter)]

    let counter = Counter::get().await;
    if counter < 10 {
        Counter::inc(10 - counter).await;
    }
    Counter::get().await
};

let coro = coro.catch0(CounterAmplifier(10));

let mut counter = CounterImpl(0);
let coro = coro.handle(&mut counter);

assert_eq!(coro.run(), 10);
assert_eq!(counter.0, 100);
```

## TODO

- [x] `handler!(Eff1(x) | Eff2(x) => todo!())`
  - [x] Expand root `continue` to `ControlFlow::Continue`
  - [x] Effectful handlers (`transform`)
- [x] `#[group]` on trait definition
- [x] `#[group_handler]` on trait implementation
  - [x] Effectful group handlers
- [ ] `catch!(expr.await { Eff1(x) => todo!() })` (In-place catch)