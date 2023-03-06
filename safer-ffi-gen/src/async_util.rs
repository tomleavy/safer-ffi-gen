use once_cell::sync::Lazy;
use tokio::runtime::Runtime;

pub static BLOCKING_ASYNC_RUNTIME: Lazy<Runtime> = Lazy::new(|| Runtime::new().unwrap());
