#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

mod core;
mod impls;

pub use core::{Deserialize, Serialize};

pub mod internal {
    pub use crate::core::{Error, Number, Result, Value};

    pub mod sys {
        pub use alloc::collections::BTreeMap;
        pub use alloc::format;
        pub use alloc::string::{String, ToString};
        pub use alloc::vec::Vec;
    }
}
