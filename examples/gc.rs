#![feature(coroutines, coroutine_trait)]

use std::{
    alloc::Layout,
    collections::HashMap,
    mem::{self, ManuallyDrop},
    num::NonZeroUsize,
    ops::{Deref, DerefMut},
    ptr::NonNull,
    sync::atomic,
};

use reffect::{
    Effect, Effectful, EffectfulExt, catch,
    effect::{EffectList, ResumeTy},
    effectful, effectful_block,
    util::{
        sum_type::{Rem, RemTags, range::SplitList, repr::Split},
        tag::Tag,
    },
};

#[repr(transparent)]
struct Gc<T: ?Sized, const UNIQUE: bool = false>(*mut T);

impl<T: ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc(self.0)
    }
}

impl<T: ?Sized, const UNIQUE: bool> Gc<T, UNIQUE> {
    fn into_shared(self) -> Gc<T> {
        Gc(ManuallyDrop::new(self).0)
    }
}

impl<T: ?Sized, const UNIQUE: bool> Drop for Gc<T, UNIQUE> {
    fn drop(&mut self) {
        self.0 = self.0.with_addr(0);
        atomic::compiler_fence(atomic::Ordering::Release);
        println!("dropping reference of {self:p}");
    }
}

impl<T, const UNIQUE: bool> Deref for Gc<T, UNIQUE> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

impl<T> DerefMut for Gc<T, true> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut *self.0 }
    }
}

#[derive(Debug, Clone, Copy)]
struct Alloc {
    layout: Layout,
    zero: bool,
    drop: unsafe fn(NonNull<()>),
}

impl Effect for Alloc {
    type Resume = NonNull<()>;
}

#[effectful(Alloc)]
fn gc_unique<T>(t: T) -> Gc<T, true> {
    unsafe fn drop_ptr<T>(ptr: NonNull<()>) {
        unsafe { core::ptr::drop_in_place(ptr.as_ptr().cast::<T>()) }
    }

    let ptr = yield Alloc {
        layout: Layout::new::<T>(),
        zero: false,
        drop: drop_ptr::<T>,
    };
    let ptr = ptr.cast::<T>();
    unsafe { ptr.as_ptr().write(t) };

    Gc(ptr.as_ptr())
}

#[effectful(Alloc)]
fn gc<T>(t: T) -> Gc<T> {
    gc_unique(t).await.into_shared()
}

/// # Safety
///
/// - `allocate` must allocate memory according the request in `Alloc` and
///   return a valid pointer to the allocated memory.
unsafe trait Collector {
    unsafe fn allocate(&mut self, alloc: Alloc) -> Option<NonNull<()>>;

    unsafe fn mark(&mut self, address: NonZeroUsize);

    unsafe fn reclaim(&mut self);
}

trait CollectExt<E, U>: Effectful<E> + Sized
where
    E: EffectList + Split<Alloc, U>,
    E::ResumeList: Split<ResumeTy<Alloc>, U, Remainder = <Rem<E, Alloc, U> as EffectList>::ResumeList>
        + SplitList<
            Rem<E::ResumeList, ResumeTy<Alloc>, U>,
            RemTags<E::ResumeList, ResumeTy<Alloc>, U>,
        >,
    U: Tag,
    Rem<E, Alloc, U>: EffectList,
{
    #[effectful(static; ...Rem<E, Alloc, U>)]
    fn collect<C: Collector>(self, collector: &mut C) -> Self::Return {
        let this = self;
        let mut reclaimed = false;
        let coro = core::pin::pin!(this);

        let ptr = (&*coro.as_ref() as *const Self).cast::<usize>();
        assert!(ptr.is_aligned());

        catch!(coro.await {
            #![non_exhaustive]

            alloc @ Alloc { .. } => loop {
                if let Some(ptr) = unsafe { collector.allocate(alloc) } {
                    reclaimed = false;
                    break ptr;
                }

                if !reclaimed {
                    unsafe {
                        (0..(mem::size_of::<Self>() / mem::size_of::<usize>()))
                            .filter_map(|delta| {
                                let value = ptr.add(delta).read();
                                println!("{:p} = {:#x}", ptr.add(delta), value);
                                NonZeroUsize::new(value)
                            })
                            .for_each(|addr| collector.mark(addr));

                        collector.reclaim();
                    }
                    reclaimed = true;
                } else {
                    panic!("memory exhausted")
                }
            }
        })
    }
}

impl<Coro, E, U> CollectExt<E, U> for Coro
where
    Coro: Effectful<E>,
    E: EffectList + Split<Alloc, U>,
    E::ResumeList: Split<ResumeTy<Alloc>, U, Remainder = <Rem<E, Alloc, U> as EffectList>::ResumeList>
        + SplitList<
            Rem<E::ResumeList, ResumeTy<Alloc>, U>,
            RemTags<E::ResumeList, ResumeTy<Alloc>, U>,
        >,
    U: Tag,
    Rem<E, Alloc, U>: EffectList,
{
}

struct AllocData {
    ptr: NonNull<()>,
    layout: Layout,
    ref_count: usize,
    drop: unsafe fn(NonNull<()>),
}

struct SimpleCollector {
    map: HashMap<NonZeroUsize, AllocData>,
    heap_size: usize,
    heap_limit: usize,
}

impl SimpleCollector {
    fn new(heap_limit: usize) -> Self {
        Self {
            map: HashMap::new(),
            heap_size: 0,
            heap_limit,
        }
    }
}

unsafe impl Collector for SimpleCollector {
    unsafe fn allocate(&mut self, alloc: Alloc) -> Option<NonNull<()>> {
        if self.heap_size + alloc.layout.size() > self.heap_limit {
            println!("heap is full");
            return None;
        }

        let ptr = if alloc.zero {
            unsafe { std::alloc::alloc_zeroed(alloc.layout) }
        } else {
            unsafe { std::alloc::alloc(alloc.layout) }
        };
        let ptr = NonNull::new(ptr.cast())?;
        println!("allocated {:?} of size {:?}", ptr, alloc.layout.size());

        self.heap_size += alloc.layout.size();
        self.map.insert(ptr.addr(), AllocData {
            ptr,
            layout: alloc.layout,
            ref_count: 0,
            drop: alloc.drop,
        });
        Some(ptr)
    }

    unsafe fn mark(&mut self, address: NonZeroUsize) {
        if let Some(data) = self.map.get_mut(&address) {
            println!("marking {:#x}", address);
            data.ref_count += 1;
        }
    }

    unsafe fn reclaim(&mut self) {
        self.map.retain(|_, data| {
            let referred = mem::replace(&mut data.ref_count, 0) > 0;
            if !referred {
                println!("dropping {:p}", data.ptr);
                unsafe { (data.drop)(data.ptr) };
                self.heap_size -= data.layout.size();
                unsafe { std::alloc::dealloc(data.ptr.as_ptr().cast(), data.layout) };
            }
            referred
        })
    }
}

fn main() {
    let f = effectful_block! {
        #![effectful(Alloc)]

        let obj2 = {
            let obj1: Gc<i32> = gc(1000).await;
            println!("obj1 is at {:p}", &obj1);
            let obj1_clone = obj1.clone();
            println!("obj1_clone is at {:p}", &obj1_clone);
            let obj2 = gc_unique(67890).await;
            println!("obj2 is at {:p}", &obj2);
            obj2
        };
        let obj3 = gc_unique(1).await;
        println!("obj3 is at {:p}", &obj3);
        *obj2 + *obj3
    };

    // We limit the heap to be able to contain only 2 integers, whereas the block
    // allocates 3 integer objects to force the mark-sweeping process.
    let mut c = SimpleCollector::new(2 * mem::size_of::<i32>());
    assert_eq!(f.collect(&mut c).run(), 67891);
}
