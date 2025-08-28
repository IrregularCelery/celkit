#![cfg_attr(not(feature = "std"), no_std)]

pub use celkit_core::{Deserialize, Serialize};

pub mod core {
    pub use celkit_core::internal::sys::*;
    pub use celkit_core::internal::utils;
    pub use celkit_core::internal::{Error, Result, Value};
}

#[cfg(feature = "string")]
pub use celkit_string::{from_str, to_mini, to_pretty, to_string};

#[cfg(feature = "derive")]
pub use celkit_derive::{Deserialize, Serialize};
