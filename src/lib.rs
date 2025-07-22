mod core;
mod impls;

pub use core::{Deserialize, Serialize};

pub mod internal {
    pub use crate::core::{Error, Number, Result, Value};
}
