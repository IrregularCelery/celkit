use crate::core::{Deserialize, Serialize};
use crate::internal::sys::*;
use crate::internal::{Error, Number, Result, Value};

// ------------------------------- Helpers -------------------------------- //

macro_rules! impl_for_integer {
    ($type:ty, $variant:ident) => {
        impl Serialize for $type {
            fn serialize(&self) -> Result<Value> {
                Ok(Value::Number(Number::$variant(*self)))
            }
        }

        impl Deserialize for $type {
            fn deserialize(value: Value) -> Result<Self> {
                match value {
                    Value::Number(Number::$variant(number)) => Ok(number),
                    Value::Number(number) => {
                        // Try to convert from other numeric types
                        match number {
                            Number::U8(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `u8` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I8(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `i8` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U16(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `u16` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I16(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `i16` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U32(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `u32` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I32(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `i32` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U64(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `u64` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I64(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `i64` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U128(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `u128` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I128(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert `i128` number {} to `{}`",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::F32(n) => {
                                if !n.is_finite() {
                                    return Err(Error::new(format!(
                                        "Cannot convert non-finite `f32` {} to integer type `{}`",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                if n < <$type>::MIN as f32 || n > <$type>::MAX as f32 {
                                    return Err(Error::new(format!(
                                        "`f32` number {} exceeds the bounds of {}",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                let n_int = n as $type;

                                if n_int as f32 != n {
                                    return Err(Error::new(format!(
                                        "Cannot convert `f32` number {} to `{}` \
                                         without loss of precision",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                Ok(n_int)
                            }
                            Number::F64(n) => {
                                if !n.is_finite() {
                                    return Err(Error::new(format!(
                                        "Cannot convert non-finite `f64` {} to integer type `{}`",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                if n < <$type>::MIN as f64 || n > <$type>::MAX as f64 {
                                    return Err(Error::new(format!(
                                        "`f64` number {} exceeds the bounds of `{}`",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                let n_int = n as $type;

                                if n_int as f64 != n {
                                    return Err(Error::new(format!(
                                        "Cannot convert `f64` number {} to `{}` \
                                         without loss of precision",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                Ok(n_int)
                            }
                        }
                    }
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

// ------------------------------- Integer -------------------------------- //

impl_for_integer!(u8, U8);
impl_for_integer!(i8, I8);
impl_for_integer!(u16, U16);
impl_for_integer!(i16, I16);
impl_for_integer!(u32, U32);
impl_for_integer!(i32, I32);
impl_for_integer!(u64, U64);
impl_for_integer!(i64, I64);
impl_for_integer!(u128, U128);
impl_for_integer!(i128, I128);

// -------------------------------- Float --------------------------------- //

impl Serialize for f32 {
    fn serialize(&self) -> Result<Value> {
        Ok(Value::Number(Number::F32(*self)))
    }
}

impl Serialize for f64 {
    fn serialize(&self) -> Result<Value> {
        Ok(Value::Number(Number::F64(*self)))
    }
}

impl Deserialize for f32 {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Number(Number::F32(number)) => Ok(number),
            Value::Number(Number::F64(number)) => {
                if number.is_infinite() {
                    if number.is_sign_positive() {
                        return Ok(f32::INFINITY);
                    }

                    return Ok(f32::NEG_INFINITY);
                }

                if number.is_nan() {
                    return Ok(f32::NAN);
                }

                let number_f32 = number as f32;

                if number.is_finite() && number_f32.is_infinite() {
                    return Err(Error::new(format!(
                        "`f64` value {} exceeds the bounds of `f32`",
                        number
                    )));
                }

                if number != 0.0 && number_f32 == 0.0 {
                    return Err(Error::new(format!(
                        "`f64` value {} underflowed to zero for `f32`",
                        number
                    )));
                }

                Ok(number_f32)
            }
            Value::Number(number) => {
                // Convert integers to f32
                let n = match number {
                    Number::U8(n) => n as f32,
                    Number::I8(n) => n as f32,
                    Number::U16(n) => n as f32,
                    Number::I16(n) => n as f32,
                    Number::U32(n) => {
                        let n_f32 = n as f32;

                        if n_f32.is_infinite() {
                            return Err(Error::new(format!(
                                "`u32` value {} exceeds the bounds of `f32`",
                                n
                            )));
                        }

                        if n_f32 as u32 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `u32` number {} to `f32` without loss of precision",
                                n,
                            )));
                        }

                        n_f32
                    }
                    Number::I32(n) => {
                        let n_f32 = n as f32;

                        if n_f32.is_infinite() {
                            return Err(Error::new(format!(
                                "`i32` value {} exceeds the bounds of `f32`",
                                n
                            )));
                        }

                        if n_f32 as i32 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `i32` number {} to `f32` without loss of precision",
                                n,
                            )));
                        }

                        n_f32
                    }
                    Number::U64(n) => {
                        let n_f32 = n as f32;

                        if n_f32.is_infinite() {
                            return Err(Error::new(format!(
                                "`u64` value {} exceeds the bounds of `f32`",
                                n
                            )));
                        }

                        if n_f32 as u64 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `u64` number {} to `f32` without loss of precision",
                                n,
                            )));
                        }

                        n_f32
                    }
                    Number::I64(n) => {
                        let n_f32 = n as f32;

                        if n_f32.is_infinite() {
                            return Err(Error::new(format!(
                                "`i64` value {} exceeds the bounds of `f32`",
                                n
                            )));
                        }

                        if n_f32 as i64 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `i64` number {} to `f32` without loss of precision",
                                n,
                            )));
                        }

                        n_f32
                    }
                    Number::U128(n) => {
                        let n_f32 = n as f32;

                        if n_f32.is_infinite() {
                            return Err(Error::new(format!(
                                "`u128` value {} exceeds the bounds of `f32`",
                                n
                            )));
                        }

                        if n_f32 as u128 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `u128` number {} to `f32` without loss of precision",
                                n,
                            )));
                        }

                        n_f32
                    }
                    Number::I128(n) => {
                        let n_f32 = n as f32;

                        if n_f32.is_infinite() {
                            return Err(Error::new(format!(
                                "`i128` value {} exceeds the bounds of `f32`",
                                n
                            )));
                        }

                        if n_f32 as i128 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `i128` number {} to `f32` without loss of precision",
                                n,
                            )));
                        }

                        n_f32
                    }
                    _ => unreachable!(),
                };

                Ok(n)
            }
            _ => Err(Error::new("Expected `f32` number")),
        }
    }
}

impl Deserialize for f64 {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Number(Number::F64(number)) => Ok(number),
            Value::Number(Number::F32(number)) => {
                if number.is_infinite() {
                    if number.is_sign_positive() {
                        return Ok(f64::INFINITY);
                    }

                    return Ok(f64::NEG_INFINITY);
                }

                if number.is_nan() {
                    return Ok(f64::NAN);
                }

                Ok(number as f64)
            }
            Value::Number(number) => {
                // Convert integers to f64
                let n = match number {
                    Number::U8(n) => n as f64,
                    Number::I8(n) => n as f64,
                    Number::U16(n) => n as f64,
                    Number::I16(n) => n as f64,
                    Number::U32(n) => n as f64,
                    Number::I32(n) => n as f64,
                    Number::U64(n) => {
                        let n_f64 = n as f64;

                        if n_f64.is_infinite() {
                            return Err(Error::new(format!(
                                "`u64` value {} exceeds the bounds of `f64`",
                                n
                            )));
                        }

                        if n_f64 as u64 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `u64` number {} to `f64` without loss of precision",
                                n,
                            )));
                        }

                        n_f64
                    }
                    Number::I64(n) => {
                        let n_f64 = n as f64;

                        if n_f64.is_infinite() {
                            return Err(Error::new(format!(
                                "`i64` value {} exceeds the bounds of `f64`",
                                n
                            )));
                        }

                        if n_f64 as i64 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `i64` number {} to `f64` without loss of precision",
                                n,
                            )));
                        }

                        n_f64
                    }
                    Number::U128(n) => {
                        let n_f64 = n as f64;

                        if n_f64.is_infinite() {
                            return Err(Error::new(format!(
                                "`u128` value {} exceeds the bounds of `f64`",
                                n
                            )));
                        }

                        if n_f64 as u128 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `u128` number {} to `f64` without loss of precision",
                                n,
                            )));
                        }

                        n_f64
                    }
                    Number::I128(n) => {
                        let n_f64 = n as f64;

                        if n_f64.is_infinite() {
                            return Err(Error::new(format!(
                                "`i128` value {} exceeds the bounds of `f64`",
                                n
                            )));
                        }

                        if n_f64 as i128 != n {
                            return Err(Error::new(format!(
                                "Cannot convert `i128` number {} to `f64` without loss of precision",
                                n,
                            )));
                        }

                        n_f64
                    }
                    _ => unreachable!(),
                };

                Ok(n)
            }
            _ => Err(Error::new("Expected `f64` number")),
        }
    }
}

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
        let mut values = BTreeMap::new();

        match self {
            Ok(value) => values.insert("Ok".to_string(), value.serialize()?),
            Err(error) => values.insert("Err".to_string(), error.serialize()?),
        };

        Ok(Value::Object(values))
    }
}

impl<T: Deserialize, E: Deserialize> Deserialize for core::result::Result<T, E> {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Object(mut object) => {
                if object.len() != 1 {
                    return Err(Error::new(
                        "Expected `Result` object with exactly one entry (`Ok` or `Err`)",
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
            _ => Err(Error::new("Expected `Result` object")),
        }
    }
}

// ------------------------------ Duration -------------------------------- //

#[cfg(feature = "std")]
impl Serialize for std::time::Duration {
    fn serialize(&self) -> Result<Value> {
        let mut values = BTreeMap::new();

        values.insert(
            "secs".to_string(),
            Value::Number(Number::U64(self.as_secs())),
        );
        values.insert(
            "nanos".to_string(),
            Value::Number(Number::U32(self.subsec_nanos())),
        );

        Ok(Value::Object(values))
    }
}

#[cfg(feature = "std")]
impl Deserialize for std::time::Duration {
    fn deserialize(value: Value) -> Result<Self> {
        match value {
            Value::Object(mut object) => {
                if object.len() != 2 {
                    return Err(Error::new(
                        "Expected `Duration` object with exactly two entries (`secs` and `nanos`)",
                    ));
                }

                let secs = match object.remove("secs") {
                    Some(Value::Number(number)) => match number {
                        Number::U8(n) => n as u64,
                        Number::I8(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i8` number {} to `u64` for `secs`",
                                n,
                            ))
                        })?,
                        Number::U16(n) => n as u64,
                        Number::I16(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i16` number {} to `u64` for `secs`",
                                n,
                            ))
                        })?,
                        Number::U32(n) => n as u64,
                        Number::I32(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i32` number {} to `u64` for `secs`",
                                n,
                            ))
                        })?,
                        Number::U64(n) => n,
                        Number::I64(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i64` number {} to `u64` for `secs`",
                                n,
                            ))
                        })?,
                        Number::U128(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `u128` number {} to `u64` for `secs`",
                                n,
                            ))
                        })?,
                        Number::I128(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i128` number {} to `u64` for `secs`",
                                n,
                            ))
                        })?,
                        Number::F32(n) => {
                            if n < 0.0 || n.is_infinite() || n.is_nan() {
                                return Err(Error::new(format!(
                                    "`secs` must be a finite non-negative number: {}",
                                    n
                                )));
                            }

                            if n > u64::MAX as f32 {
                                return Err(Error::new(format!(
                                    "`secs` exceeds the bounds for `Duration`: {}",
                                    n
                                )));
                            }

                            n as u64
                        }
                        Number::F64(n) => {
                            if n < 0.0 || n.is_infinite() || n.is_nan() {
                                return Err(Error::new(format!(
                                    "`secs` must be a finite non-negative number: {}",
                                    n
                                )));
                            }

                            if n > u64::MAX as f64 {
                                return Err(Error::new(format!(
                                    "`secs` exceeds the bounds for `Duration`: {}",
                                    n
                                )));
                            }

                            n as u64
                        }
                    },
                    _ => {
                        return Err(Error::new(
                            "Expected `Duration` object with a valid numeric `secs` entry",
                        ))
                    }
                };

                let nanos = match object.remove("nanos") {
                    Some(Value::Number(number)) => match number {
                        Number::U8(n) => n as u32,
                        Number::I8(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i8` number {} to `u32` for `nanos`",
                                n,
                            ))
                        })?,
                        Number::U16(n) => n as u32,
                        Number::I16(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i16` number {} to `u32` for `nanos`",
                                n,
                            ))
                        })?,
                        Number::U32(n) => n,
                        Number::I32(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i32` number {} to `u32` for `nanos`",
                                n,
                            ))
                        })?,
                        Number::U64(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `u64` number {} to `u32` for `nanos`",
                                n,
                            ))
                        })?,
                        Number::I64(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i64` number {} to `u32` for `nanos`",
                                n,
                            ))
                        })?,
                        Number::U128(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `u128` number {} to `u32` for `nanos`",
                                n,
                            ))
                        })?,
                        Number::I128(n) => n.try_into().map_err(|_| {
                            Error::new(format!(
                                "Cannot convert `i128` number {} to `u32` for `nanos`",
                                n,
                            ))
                        })?,
                        Number::F32(n) => {
                            if n < 0.0 || n.is_infinite() || n.is_nan() {
                                return Err(Error::new(format!(
                                    "`nanos` must be a finite non-negative number: {}",
                                    n
                                )));
                            }

                            if n > u32::MAX as f32 {
                                return Err(Error::new(format!(
                                    "`nanos` exceeds the bounds for `Duration`: {}",
                                    n
                                )));
                            }

                            n as u32
                        }
                        Number::F64(n) => {
                            if n < 0.0 || n.is_infinite() || n.is_nan() {
                                return Err(Error::new(format!(
                                    "`nanos` must be a finite non-negative number: {}",
                                    n
                                )));
                            }

                            if n > u32::MAX as f64 {
                                return Err(Error::new(format!(
                                    "`nanos` exceeds the bounds for `Duration`: {}",
                                    n
                                )));
                            }

                            n as u32
                        }
                    },
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
            _ => Err(Error::new("Expected `Duration` object")),
        }
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

                let mut fields = $crate::internal::sys::Vec::with_capacity(EXPECTED_LEN);

                $(
                    fields.push(
                        (
                            $crate::internal::utils::unescape_identifier(
                                stringify!($field_name)
                            ).to_string(),
                            self.$field_name.serialize()?
                        )
                    );
                )*

                Ok($crate::internal::Value::Struct(fields))
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
