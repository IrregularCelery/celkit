#![cfg_attr(not(feature = "std"), no_std)]

extern crate alloc;

mod core;
mod impls;
mod utils;

pub use core::{Deserialize, Serialize};

pub mod internal {
    pub use crate::core::{Error, Number, Result, Value};

    pub mod sys {
        pub use alloc::boxed::Box;
        pub use alloc::collections::{BTreeMap, VecDeque};
        pub use alloc::format;
        pub use alloc::string::{String, ToString};
        pub use alloc::vec::Vec;
    }

    pub mod utils {
        pub use crate::utils::unescape_identifier;
    }
}
