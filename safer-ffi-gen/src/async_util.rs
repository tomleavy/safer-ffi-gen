use alloc::boxed::Box;
use core::{
    future::Future,
    pin::{pin, Pin},
};
use once_cell::sync::OnceCell;

pub trait Executor: Send + Sync + 'static {
    fn block_on(&self, f: Pin<&mut dyn Future<Output = ()>>);
}

impl<F> Executor for F
where
    F: Fn(Pin<&mut dyn Future<Output = ()>>) + Send + Sync + 'static,
{
    fn block_on(&self, f: Pin<&mut dyn Future<Output = ()>>) {
        self(f);
    }
}

static EXECUTOR: OnceCell<Box<dyn Executor>> = OnceCell::new();

pub fn block_on<F: Future>(f: F) -> F::Output {
    let output = OnceCell::new();
    {
        let f = pin!(async {
            output.set(f.await).ok().unwrap();
        });
        EXECUTOR
            .get()
            .expect("Async executor must be set once with safer_ffi_gen::set_executor")
            .block_on(f);
    }
    output.into_inner().unwrap()
}

pub fn set_executor<T: Executor>(executor: T) {
    EXECUTOR
        .set(Box::new(executor))
        .ok()
        .expect("Async executor already set");
}

pub fn set_block_on_executor<F>(f: F)
where
    F: Fn(Pin<&mut dyn Future<Output = ()>>) + Send + Sync + 'static,
{
    set_executor(f);
}
