// NOTE:
//
// Special-type serialization
//
// Some special types (e.g. Result, Duration) are represented as structs with reserved field names
// that tell encoders how to handle them.
//
// Reserved field names:
// - "0": A marker indicating the struct is "special" and must be handled differently by encoders.
//   (This field is always present in special structs)
// - "#": Type-specific discriminants or values that are ignored by verbose encoders (e.g. string),
//   but kept by data-only encoders (e.g. binary).
//   If only one such field exists, it can be flattened instead of wrapped.
// - "%": Values that are the opposite of "#"; they are ignored by data-only encoders (e.g. binary),
//   but kept by verbose encoders (e.g. string).
//   If only one such field exists, it can be flattened instead of wrapped.
// - Normal names ("Ok", "secs", ...): Used by verbose encoders. For data-only encoders, the names
//   are ignored, but the values are kept in positional order.
//
// This design ensures any encoder can have access to necessary values and data while having the
// ability to ignore data that is unnecessary to them.
//
// The "0", "#", and "%" names were chosen to guarantee no conflicts with user-defined field names.

use crate::core::{Deserialize, Serialize};
use crate::internal::sys::*;
use crate::internal::{Error, Number, Result, Value};

// ------------------------------- Helpers -------------------------------- //

macro_rules! impl_for_number {
    ($type:ty, $variant:ident, $method:ident) => {
        impl Serialize for $type {
            fn serialize(&self) -> Result<Value> {
                Ok(Value::Number(Number::$variant(*self)))
            }
        }

        impl Deserialize for $type {
            fn deserialize(value: Value) -> Result<Self> {
                match value {
                    Value::Number(number) => number.$method(),
                    _ => Err(Error::new(format!(
                        "Expected `{}` number",
                        stringify!($type)
                    ))),
                }
            }
        }
    };
}

macro_rules! impl_for_tuple {
    ($($member:ident),+) => {
        impl<$($member: Serialize),+> Serialize for ($($member,)+) {
            fn serialize(&self) -> Result<Value> {
                #[allow(non_snake_case)]
                let ($($member,)+) = self;

                Ok(Value::Tuple(Vec::from([$($member.serialize()?),+])))
            }
        }

        impl<$($member: Deserialize),+> Deserialize for ($($member,)+) {
            fn deserialize(value: Value) -> Result<Self> {
                match value {
                    Value::Tuple(tuple) => {
                        const EXPECTED_LEN: usize = 0
                            $(+ { let _ = stringify!($member); 1 })*;

                        if tuple.len() != EXPECTED_LEN {
                            return Err(Error::new(format!(
                                "Expected `tuple` with {} members, got {}",
                                EXPECTED_LEN,
                                tuple.len()
                            )));
                        }

                        let mut tuple_iter = tuple.into_iter();

                        Ok(($(
                            $member::deserialize(tuple_iter.next().expect(
                                "This SHOULD never happen because of length check!"
                            ))?
                        ),+,))
                    }
                    _ => Err(Error::new("Expected `tuple`")),
                }
            }
        }
    };
}

// ------------------------------- Boolean -------------------------------- //

impl Serialize for bool {
    fn serialize(&self) -> Result<Value> {
        Ok(Value::Boolean(*self))
    }
}

impl Deserialize for bool {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Boolean(bool) => Ok(bool),
            _ => Err(Error::new("Expected `bool`")),
        }
    }
}

// ------------------------------- Number --------------------------------- //

impl_for_number!(u8, U8, as_u8);
impl_for_number!(i8, I8, as_i8);
impl_for_number!(u16, U16, as_u16);
impl_for_number!(i16, I16, as_i16);
impl_for_number!(u32, U32, as_u32);
impl_for_number!(i32, I32, as_i32);
impl_for_number!(u64, U64, as_u64);
impl_for_number!(i64, I64, as_i64);
impl_for_number!(u128, U128, as_u128);
impl_for_number!(i128, I128, as_i128);
impl_for_number!(usize, Usize, as_usize);
impl_for_number!(isize, Isize, as_isize);
impl_for_number!(f32, F32, as_f32);
impl_for_number!(f64, F64, as_f64);

// -------------------------------- char ---------------------------------- //

impl Serialize for char {
    fn serialize(&self) -> Result<Value> {
        Ok(Value::Text(self.to_string()))
    }
}

impl Deserialize for char {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Text(string) => {
                let mut chars = string.chars();

                match (chars.next(), chars.next()) {
                    (Some(c), None) => Ok(c),
                    (None, _) => Err(Error::new(
                        "Expected `single` character, got an `empty` string",
                    )),
                    _ => Err(Error::new("Expected `single` character, got `long` text")),
                }
            }
            _ => Err(Error::new("Expected `char`")),
        }
    }
}

// -------------------------------- &str ---------------------------------- //

impl Serialize for &str {
    fn serialize(&self) -> Result<Value> {
        Ok(Value::Text(self.to_string()))
    }
}

// ------------------------------- String --------------------------------- //

impl Serialize for String {
    fn serialize(&self) -> Result<Value> {
        Ok(Value::Text(self.clone()))
    }
}

impl Deserialize for String {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Text(string) => Ok(string),
            _ => Err(Error::new("Expected `string`")),
        }
    }
}

// -------------------------- Fixed-size Array ---------------------------- //

impl<T: Serialize, const N: usize> Serialize for [T; N] {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(N);

        for item in self {
            values.push(item.serialize()?);
        }

        Ok(Value::Array(values))
    }
}

impl<T: Deserialize, const N: usize> Deserialize for [T; N] {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(array) => {
                if array.len() != N {
                    return Err(Error::new(format!(
                        "Expected fixed-size `array` of {}, got {}",
                        N,
                        array.len()
                    )));
                }

                let vec: Vec<T> = array
                    .into_iter()
                    .map(T::deserialize)
                    .collect::<Result<Vec<T>>>()?;

                vec.try_into().map_err(|_| {
                    Error::new(format!("Failed to convert `Vec` to `array` of size {}", N))
                })
            }
            _ => Err(Error::new("Expected `Fixed-size` array")),
        }
    }
}

// -------------------------------- Slice --------------------------------- //

impl<T: Serialize> Serialize for &[T] {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(self.len());

        for item in *self {
            values.push(item.serialize()?);
        }

        Ok(Value::Array(values))
    }
}

// --------------------------------- Vec ---------------------------------- //

impl<T: Serialize> Serialize for Vec<T> {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(self.len());

        for item in self {
            values.push(item.serialize()?);
        }

        Ok(Value::Array(values))
    }
}

impl<T: Deserialize> Deserialize for Vec<T> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(array) => {
                let mut vec = Vec::with_capacity(array.len());

                for value in array {
                    vec.push(T::deserialize(value)?);
                }

                Ok(vec)
            }
            _ => Err(Error::new("Expected `Vec` array")),
        }
    }
}

// ------------------------------ VecDeque -------------------------------- //

impl<T: Serialize> Serialize for VecDeque<T> {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(self.len());

        for item in self {
            values.push(item.serialize()?);
        }

        Ok(Value::Array(values))
    }
}

impl<T: Deserialize> Deserialize for VecDeque<T> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(array) => {
                let mut deque = VecDeque::with_capacity(array.len());

                for value in array {
                    deque.push_back(T::deserialize(value)?);
                }

                Ok(deque)
            }
            _ => Err(Error::new("Expected `VecDeque` array")),
        }
    }
}

// ------------------------------ BTreeSet -------------------------------- //

impl<T: Serialize> Serialize for BTreeSet<T> {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(self.len());

        for item in self {
            values.push(item.serialize()?);
        }

        Ok(Value::Array(values))
    }
}

impl<T: Deserialize + Ord> Deserialize for BTreeSet<T> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(array) => {
                let mut set = BTreeSet::new();

                for value in array {
                    set.insert(T::deserialize(value)?);
                }

                Ok(set)
            }
            _ => Err(Error::new("Expected `BTreeSet` array")),
        }
    }
}

// ------------------------------- HashSet -------------------------------- //

#[cfg(feature = "std")]
impl<T: Serialize> Serialize for std::collections::HashSet<T> {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(self.len());

        for item in self {
            values.push(item.serialize()?);
        }

        Ok(Value::Array(values))
    }
}

#[cfg(feature = "std")]
impl<T: Deserialize + core::hash::Hash + Eq> Deserialize for std::collections::HashSet<T> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(array) => {
                let mut set = std::collections::HashSet::with_capacity(array.len());

                for value in array {
                    set.insert(T::deserialize(value)?);
                }

                Ok(set)
            }
            _ => Err(Error::new("Expected `HashSet` array")),
        }
    }
}

// -------------------------------- Tuple --------------------------------- //

impl Serialize for () {
    fn serialize(&self) -> Result<Value> {
        Ok(Value::Tuple(Vec::new()))
    }
}

impl Deserialize for () {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Tuple(tuple) => {
                if tuple.len() != 0 {
                    return Err(Error::new(format!(
                        "Expected `empty` tuple, got tuple with {} members",
                        tuple.len(),
                    )));
                }

                Ok(())
            }
            _ => Err(Error::new("Expected `tuple`")),
        }
    }
}

impl_for_tuple!(T1);
impl_for_tuple!(T1, T2);
impl_for_tuple!(T1, T2, T3);
impl_for_tuple!(T1, T2, T3, T4);
impl_for_tuple!(T1, T2, T3, T4, T5);
impl_for_tuple!(T1, T2, T3, T4, T5, T6);
impl_for_tuple!(T1, T2, T3, T4, T5, T6, T7);
impl_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8);
impl_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9);
impl_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10);
impl_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11);
impl_for_tuple!(T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12);

// ------------------------------ BTreeMap -------------------------------- //

impl<K: Into<String> + Clone, V: Serialize> Serialize for BTreeMap<K, V> {
    fn serialize(&self) -> Result<Value> {
        let mut values = BTreeMap::new();

        for (key, value) in self {
            values.insert(key.clone().into(), value.serialize()?);
        }

        Ok(Value::Object(values))
    }
}

impl<V: Deserialize> Deserialize for BTreeMap<String, V> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Object(object) => {
                let mut map = BTreeMap::new();

                for (key, value) in object {
                    map.insert(key, V::deserialize(value)?);
                }

                Ok(map)
            }
            _ => Err(Error::new("Expected `BTreeMap` object")),
        }
    }
}

// ------------------------------- HashMap -------------------------------- //

#[cfg(feature = "std")]
impl<K: Into<String> + Clone, V: Serialize> Serialize for std::collections::HashMap<K, V> {
    fn serialize(&self) -> Result<Value> {
        let mut values = BTreeMap::new();

        for (key, value) in self {
            values.insert(key.clone().into(), value.serialize()?);
        }

        Ok(Value::Object(values))
    }
}

#[cfg(feature = "std")]
impl<V: Deserialize> Deserialize for std::collections::HashMap<String, V> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Object(object) => {
                let mut map = std::collections::HashMap::with_capacity(object.len());

                for (key, value) in object {
                    map.insert(key, V::deserialize(value)?);
                }

                Ok(map)
            }
            _ => Err(Error::new("Expected `HashMap` object")),
        }
    }
}

// ------------------------------- Result --------------------------------- //

impl<T: Serialize, E: Serialize> Serialize for core::result::Result<T, E> {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(3);

        // See: "Special-type serialization" at the top of this file
        values.push(("0".to_string(), Value::Null));

        match self {
            Ok(ok) => {
                values.push(("#".to_string(), Value::Number(Number::U8(1))));
                values.push(("Ok".to_string(), ok.serialize()?));
            }
            Err(err) => {
                values.push(("#".to_string(), Value::Number(Number::U8(0))));
                values.push(("Err".to_string(), err.serialize()?));
            }
        };

        Ok(Value::Struct(values))
    }
}

impl<T: Deserialize, E: Deserialize> Deserialize for core::result::Result<T, E> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(mut array) => {
                if array.len() != 2 {
                    return Err(Error::new(
                        "Expected `Result` array with exactly two items [discriminant, value] \
                        e.g. [{1/0}, value]",
                    ));
                }

                // Pop in reverse order
                let value = array
                    .pop()
                    .expect("This SHOULD never happen because of length check!");
                let discriminant = array
                    .pop()
                    .expect("This SHOULD never happen because of length check!");

                match discriminant {
                    Value::Number(number) if number.as_u8()? == 1 => Ok(Ok(T::deserialize(value)?)),
                    Value::Number(number) if number.as_u8()? == 0 => {
                        Ok(Err(E::deserialize(value)?))
                    }
                    _ => Err(Error::new("Invalid `Result` discriminant, expected 1 or 0")),
                }
            }
            Value::Tuple(mut tuple) => {
                if tuple.len() != 2 {
                    return Err(Error::new(
                        "Expected `Result` tuple with exactly two members (discriminant, value) \
                        e.g. ({1/0}, value)",
                    ));
                }

                // Pop in reverse order
                let value = tuple
                    .pop()
                    .expect("This SHOULD never happen because of length check!");
                let discriminant = tuple
                    .pop()
                    .expect("This SHOULD never happen because of length check!");

                match discriminant {
                    Value::Number(number) if number.as_u8()? == 1 => Ok(Ok(T::deserialize(value)?)),
                    Value::Number(number) if number.as_u8()? == 0 => {
                        Ok(Err(E::deserialize(value)?))
                    }
                    _ => Err(Error::new("Invalid `Result` discriminant, expected 1 or 0")),
                }
            }
            Value::Object(mut object) => {
                if object.len() != 1 {
                    return Err(Error::new(
                        "Expected `Result` object with exactly one entry `Ok` or `Err`",
                    ));
                }

                if let Some(ok) = object.remove("Ok") {
                    return Ok(Ok(T::deserialize(ok)?));
                }

                if let Some(err) = object.remove("Err") {
                    return Ok(Err(E::deserialize(err)?));
                }

                Err(Error::new(
                    "Expected `Result` object with `Ok` or `Err` entry",
                ))
            }
            _ => Err(Error::new("Expected `Result` array/tuple/object")),
        }
    }
}

// ------------------------------ Duration -------------------------------- //

#[cfg(feature = "std")]
impl Serialize for std::time::Duration {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(3);

        // See: "Special-type serialization" at the top of this file
        values.push(("0".to_string(), Value::Null));
        values.push((
            "secs".to_string(),
            Value::Number(Number::U64(self.as_secs())),
        ));
        values.push((
            "nanos".to_string(),
            Value::Number(Number::U32(self.subsec_nanos())),
        ));

        Ok(Value::Struct(values))
    }
}

#[cfg(feature = "std")]
impl Deserialize for std::time::Duration {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(mut array) => {
                if array.len() != 2 {
                    return Err(Error::new(
                        "Expected `Duration` array with exactly two items [secs, nanos]",
                    ));
                }

                // Pop in reverse order
                let nanos_value = array
                    .pop()
                    .expect("This SHOULD never happen because of length check!");
                let secs_value = array
                    .pop()
                    .expect("This SHOULD never happen because of length check!");

                let secs = match secs_value {
                    Value::Number(number) => number.as_u64()?,
                    _ => {
                        return Err(Error::new(
                            "First item of `Duration` array must be a number for `secs`",
                        ));
                    }
                };

                let nanos = match nanos_value {
                    Value::Number(number) => number.as_u32()?,
                    _ => {
                        return Err(Error::new(
                            "Second item of `Duration` array must be a number for `nanos`",
                        ))
                    }
                };

                if nanos >= 1_000_000_000 {
                    return Err(Error::new("`nanos` must be less than 1,000,000,000"));
                }

                Ok(std::time::Duration::new(secs, nanos))
            }
            Value::Tuple(mut tuple) => {
                if tuple.len() != 2 {
                    return Err(Error::new(
                        "Expected `Duration` tuple with exactly two members (secs, nanos)",
                    ));
                }

                // Pop in reverse order
                let nanos_value = tuple
                    .pop()
                    .expect("This SHOULD never happen because of length check!");
                let secs_value = tuple
                    .pop()
                    .expect("This SHOULD never happen because of length check!");

                let secs = match secs_value {
                    Value::Number(number) => number.as_u64()?,
                    _ => {
                        return Err(Error::new(
                            "First member of `Duration` tuple must be a number for `secs`",
                        ));
                    }
                };

                let nanos = match nanos_value {
                    Value::Number(number) => number.as_u32()?,
                    _ => {
                        return Err(Error::new(
                            "Second member of `Duration` tuple must be a number for `nanos`",
                        ))
                    }
                };

                if nanos >= 1_000_000_000 {
                    return Err(Error::new("`nanos` must be less than 1,000,000,000"));
                }

                Ok(std::time::Duration::new(secs, nanos))
            }
            Value::Object(mut object) => {
                if object.len() != 2 {
                    return Err(Error::new(
                        "Expected `Duration` object with exactly two entries `secs` and `nanos`",
                    ));
                }

                let secs = match object.remove("secs") {
                    Some(Value::Number(number)) => number.as_u64()?,
                    _ => {
                        return Err(Error::new(
                            "Expected `Duration` object with a valid numeric `secs` entry",
                        ))
                    }
                };

                let nanos = match object.remove("nanos") {
                    Some(Value::Number(number)) => number.as_u32()?,
                    _ => {
                        return Err(Error::new(
                            "Expected `Duration` object with a valid numeric `nanos` entry",
                        ))
                    }
                };

                if nanos >= 1_000_000_000 {
                    return Err(Error::new("`nanos` must be less than 1,000,000,000"));
                }

                Ok(std::time::Duration::new(secs, nanos))
            }
            _ => Err(Error::new("Expected `Duration` array/tuple/object")),
        }
    }
}

// ----------------------------- SystemTime ------------------------------- //

#[cfg(feature = "std")]
impl Serialize for std::time::SystemTime {
    fn serialize(&self) -> Result<Value> {
        self.duration_since(std::time::UNIX_EPOCH)
            .map_err(|value| {
                Error::new(format!(
                    "`SystemTime` value must be after `UNIX_EPOCH`, got {}",
                    value
                ))
            })?
            .serialize()
    }
}

#[cfg(feature = "std")]
impl Deserialize for std::time::SystemTime {
    fn deserialize(value: Value) -> Result<Self> {
        let duration = std::time::Duration::deserialize(value)?;

        Ok(std::time::UNIX_EPOCH + duration)
    }
}

// ------------------------------- IpAddr --------------------------------- //

impl Serialize for core::net::IpAddr {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(3);

        // See: "Special-type serialization" at the top of this file
        values.push(("0".to_string(), Value::Null));
        values.push((
            "#".to_string(),
            Value::Array({
                match self {
                    core::net::IpAddr::V4(v4) => v4
                        .octets()
                        .iter()
                        .map(|b| Value::Number(Number::U8(*b)))
                        .collect(),
                    core::net::IpAddr::V6(v6) => v6
                        .octets()
                        .iter()
                        .map(|b| Value::Number(Number::U8(*b)))
                        .collect(),
                }
            }),
        ));
        values.push(("%".to_string(), Value::Text(self.to_string())));

        Ok(Value::Struct(values))
    }
}

impl Deserialize for core::net::IpAddr {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Text(string) => string
                .parse()
                .map_err(|e| Error::new(format!("Invalid IP address: {}", e))),
            Value::Array(array) => {
                let bytes: Vec<u8> = array
                    .iter()
                    .map(|value| match value {
                        Value::Number(Number::U8(n)) => Ok(*n),
                        _ => Err(Error::new("Expected `u8` number `IpAddr` array item")),
                    })
                    .collect::<Result<_>>()?;

                match bytes.len() {
                    4 => Ok(core::net::IpAddr::V4(core::net::Ipv4Addr::new(
                        bytes[0], bytes[1], bytes[2], bytes[3],
                    ))),
                    16 => {
                        let mut array = [0u8; 16];

                        array.copy_from_slice(&bytes);

                        Ok(core::net::IpAddr::V6(core::net::Ipv6Addr::from(array)))
                    }
                    len => Err(Error::new(format!(
                        "Expected `IpAddr` array with exactly 4 (v4) or 16 (v6) items, got {}",
                        len
                    ))),
                }
            }
            Value::Tuple(tuple) => {
                let bytes: Vec<u8> = tuple
                    .iter()
                    .map(|value| match value {
                        Value::Number(Number::U8(n)) => Ok(*n),
                        _ => Err(Error::new("Expected `u8` number `IpAddr` tuple member")),
                    })
                    .collect::<Result<_>>()?;

                match bytes.len() {
                    4 => Ok(core::net::IpAddr::V4(core::net::Ipv4Addr::new(
                        bytes[0], bytes[1], bytes[2], bytes[3],
                    ))),
                    16 => {
                        let mut array = [0u8; 16];

                        array.copy_from_slice(&bytes);

                        Ok(core::net::IpAddr::V6(core::net::Ipv6Addr::from(array)))
                    }
                    len => Err(Error::new(format!(
                        "Expected `IpAddr` tuple with exactly 4 (v4) or 16 (v6) members, got {}",
                        len
                    ))),
                }
            }
            _ => Err(Error::new("Expected `IpAddr` array/tuple/string")),
        }
    }
}

// -------------------------------- Range --------------------------------- //

impl<T: Serialize> Serialize for core::ops::Range<T> {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(3);

        // See: "Special-type serialization" at the top of this file
        values.push(("0".to_string(), Value::Null));
        values.push(("start".to_string(), self.start.serialize()?));
        values.push(("end".to_string(), self.end.serialize()?));

        Ok(Value::Struct(values))
    }
}

impl<T: Deserialize> Deserialize for core::ops::Range<T> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Array(mut array) => {
                if array.len() != 2 {
                    return Err(Error::new(
                        "Expected `Range` array with exactly two items [start, end]",
                    ));
                }

                // Pop in reverse order
                let end = array
                    .pop()
                    .expect("This SHOULD never happen because of length check!");
                let start = array
                    .pop()
                    .expect("This SHOULD never happen because of length check!");

                Ok(core::ops::Range {
                    start: T::deserialize(start)?,
                    end: T::deserialize(end)?,
                })
            }
            Value::Tuple(mut tuple) => {
                if tuple.len() != 2 {
                    return Err(Error::new(
                        "Expected `Range` tuple with exactly two members (start, end)",
                    ));
                }

                // Pop in reverse order
                let end = tuple
                    .pop()
                    .expect("This SHOULD never happen because of length check!");
                let start = tuple
                    .pop()
                    .expect("This SHOULD never happen because of length check!");

                Ok(core::ops::Range {
                    start: T::deserialize(start)?,
                    end: T::deserialize(end)?,
                })
            }
            Value::Object(mut object) => {
                if object.len() != 2 {
                    return Err(Error::new(
                        "Expected `Range` object with exactly two entries `start` and `end`",
                    ));
                }

                let Some(start) = object.remove("start") else {
                    return Err(Error::new(
                        "Expected `Range` object with a valid `start` entry",
                    ));
                };

                let Some(end) = object.remove("end") else {
                    return Err(Error::new(
                        "Expected `Range` object with a valid `end` entry",
                    ));
                };

                Ok(core::ops::Range {
                    start: T::deserialize(start)?,
                    end: T::deserialize(end)?,
                })
            }
            _ => Err(Error::new("Expected `Range` array/tuple/object")),
        }
    }
}

// --------------------------- RangeInclusive ----------------------------- //

impl<T: Serialize> Serialize for core::ops::RangeInclusive<T> {
    fn serialize(&self) -> Result<Value> {
        let mut values = Vec::with_capacity(3);

        // See: "Special-type serialization" at the top of this file
        values.push(("0".to_string(), Value::Null));
        values.push(("start".to_string(), self.start().serialize()?));
        values.push(("end".to_string(), self.end().serialize()?));

        Ok(Value::Struct(values))
    }
}

impl<T: Deserialize> Deserialize for core::ops::RangeInclusive<T> {
    fn deserialize(value: Value) -> Result<Self> {
        let range = core::ops::Range::<T>::deserialize(value)?;

        Ok(range.start..=range.end)
    }
}

// --------------------------------- Box ---------------------------------- //

impl<T: Serialize> Serialize for Box<T> {
    fn serialize(&self) -> Result<Value> {
        self.as_ref().serialize()
    }
}

impl<T: Deserialize> Deserialize for Box<T> {
    fn deserialize(value: Value) -> Result<Self> {
        Ok(Box::new(T::deserialize(value)?))
    }
}

// ------------------------------- Option --------------------------------- //

impl<T: Serialize> Serialize for Option<T> {
    fn serialize(&self) -> Result<Value> {
        match self {
            Some(value) => value.serialize(),
            None => Ok(Value::Null),
        }
    }
}

impl<T: Deserialize> Deserialize for Option<T> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Null => Ok(None),
            value => Ok(Some(T::deserialize(value)?)),
        }
    }
}

// --------------------------------- Rc ----------------------------------- //

#[cfg(feature = "std")]
impl<T: Serialize> Serialize for std::rc::Rc<T> {
    fn serialize(&self) -> Result<Value> {
        self.as_ref().serialize()
    }
}

#[cfg(feature = "std")]
impl<T: Deserialize> Deserialize for std::rc::Rc<T> {
    fn deserialize(value: Value) -> Result<Self> {
        Ok(std::rc::Rc::new(T::deserialize(value)?))
    }
}

// --------------------------------- Arc ---------------------------------- //

#[cfg(feature = "std")]
impl<T: Serialize> Serialize for std::sync::Arc<T> {
    fn serialize(&self) -> Result<Value> {
        self.as_ref().serialize()
    }
}

#[cfg(feature = "std")]
impl<T: Deserialize> Deserialize for std::sync::Arc<T> {
    fn deserialize(value: Value) -> Result<Self> {
        Ok(std::sync::Arc::new(T::deserialize(value)?))
    }
}

// -------------------------------- Cell ---------------------------------- //

#[cfg(feature = "std")]
impl<T: Serialize + Copy> Serialize for std::cell::Cell<T> {
    fn serialize(&self) -> Result<Value> {
        self.get().serialize()
    }
}

#[cfg(feature = "std")]
impl<T: Deserialize> Deserialize for std::cell::Cell<T> {
    fn deserialize(value: Value) -> Result<Self> {
        Ok(std::cell::Cell::new(T::deserialize(value)?))
    }
}

// ------------------------------- RefCell -------------------------------- //

#[cfg(feature = "std")]
impl<T: Serialize> Serialize for std::cell::RefCell<T> {
    fn serialize(&self) -> Result<Value> {
        self.try_borrow()
            .map_err(|_| Error::new("`RefCell` is already borrowed mutably"))?
            .serialize()
    }
}

#[cfg(feature = "std")]
impl<T: Deserialize> Deserialize for std::cell::RefCell<T> {
    fn deserialize(value: Value) -> Result<Self> {
        Ok(std::cell::RefCell::new(T::deserialize(value)?))
    }
}

// ------------------------------- Struct --------------------------------- //

#[macro_export]
macro_rules! impl_for_struct {
    (
        $(#[$attr:meta])*
        $vis:vis struct $name:ident {
            $(
                $field_vis:vis $field_name:ident: $field_type:ty
            ),* $(,)?
        }
    ) => {
        // Forward the struct definition
        $(#[$attr])*
        $vis struct $name {
            $(
                $field_vis $field_name: $field_type
            ),*
        }

        impl $crate::Serialize for $name {
            fn serialize(&self) -> $crate::internal::Result<$crate::internal::Value> {
                const EXPECTED_LEN: usize = 0 $(+ { let _ = stringify!($field_name); 1 })*;

                let mut values = $crate::internal::sys::Vec::with_capacity(EXPECTED_LEN);

                $(
                    values.push(
                        (
                            $crate::internal::utils::unescape_identifier(
                                stringify!($field_name)
                            ).to_string(),
                            self.$field_name.serialize()?
                        )
                    );
                )*

                Ok($crate::internal::Value::Struct(values))
            }
        }

        impl $crate::Deserialize for $name {
            fn deserialize(value: $crate::internal::Value) -> $crate::internal::Result<Self> {
                match value {
                    $crate::internal::Value::Struct(fields) => {
                        let expected_fields = [$(
                            $crate::internal::utils::unescape_identifier(stringify!($field_name))
                        ),*];

                        let mut is_positional = false;

                        if fields.len() == expected_fields.len() {
                            is_positional = true;

                            for (i, (field_name, field_value)) in fields.iter().enumerate() {
                                if field_name.is_empty() {
                                    // Order must be correct or error
                                    break;
                                }

                                if expected_fields[i] != field_name {
                                    // Order isn't correct
                                    is_positional = false;

                                    break;
                                }
                            }
                        }

                        // Ordered, we match the expected fields
                        if is_positional {
                            let mut fields_iter = fields.into_iter();

                            $(
                                let $field_name = {
                                    let (_, field_value) = fields_iter
                                        .next()
                                        .expect(
                                            "This SHOULD never happen because of length check!"
                                        );

                                    <$field_type>::deserialize(field_value)?
                                };
                            )*


                            return Ok($name {
                                $(
                                    $field_name
                                ),*
                            });
                        }


                        $(
                            let mut $field_name: Option<$field_type> = None;
                        )*

                        // Unordered, we search for the values by names
                        for (field_name, field_value) in fields {
                            $(
                                if field_name
                                    == $crate::internal::utils::unescape_identifier(
                                        stringify!($field_name)
                                    )
                                {
                                    $field_name = Some(<$field_type>::deserialize(field_value)?);

                                    continue;
                                }
                            )*

                            // Unknown field, do nothing
                        }

                        // Check whether all of the fields were found
                        $(
                            let $field_name = $field_name.ok_or_else(|| {
                                $crate::internal::Error::new(format!(
                                    "Missing `{}` field in `{}`",
                                    $crate::internal::utils::unescape_identifier(
                                        stringify!($field_name)
                                    ),
                                    stringify!($name),
                                ))
                            })?;
                        )*

                        Ok($name {
                            $(
                                $field_name
                            ),*
                        })
                    }
                    _ => Err($crate::internal::Error::new(
                        format!("Expected `{}` struct", stringify!($name))
                    ))
                }
            }
        }
    };
}
