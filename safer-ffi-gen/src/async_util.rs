use once_cell::sync::Lazy;
use std::future::Future;
use tokio::runtime::Runtime;

static BLOCKING_ASYNC_RUNTIME: Lazy<Runtime> = Lazy::new(|| Runtime::new().unwrap());

pub fn block_on<F: Future>(f: F) -> F::Output {
    BLOCKING_ASYNC_RUNTIME.block_on(f)
}
