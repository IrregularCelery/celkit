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
                                    "Cannot convert u8 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I8(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert i8 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U16(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert u16 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I16(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert i16 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U32(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert u32 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I32(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert i32 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U64(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert u64 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I64(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert i64 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::U128(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert u128 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::I128(n) => <$type>::try_from(n).map_err(|_| {
                                Error::new(format!(
                                    "Cannot convert i128 number {} to {}",
                                    n,
                                    stringify!($type)
                                ))
                            }),
                            Number::F32(n) => {
                                let as_int = n as i128;

                                if as_int as f32 != n {
                                    return Err(Error::new(format!(
                                        "Cannot convert f32 number {} to {} \
                                         without loss of precision",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                <$type>::try_from(as_int).map_err(|_| {
                                    Error::new(format!(
                                        "Cannot convert f32 number {} to {}",
                                        n,
                                        stringify!($type)
                                    ))
                                })
                            }
                            Number::F64(n) => {
                                let as_int = n as i128;

                                if as_int as f64 != n {
                                    return Err(Error::new(format!(
                                        "Cannot convert f64 number {} to {} \
                                         without loss of precision",
                                        n,
                                        stringify!($type)
                                    )));
                                }

                                <$type>::try_from(as_int).map_err(|_| {
                                    Error::new(format!(
                                        "Cannot convert f64 number {} to {}",
                                        n,
                                        stringify!($type)
                                    ))
                                })
                            }
                        }
                    }
                    _ => Err(Error::new(format!(
                        "Expected number for {}",
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
                    Value::Tuple(mut tuple) => {
                        const EXPECTED_LEN: usize = 0
                            $(+ { let _ = stringify!($member); 1 })*;

                        if tuple.len() != EXPECTED_LEN {
                            return Err(Error::new(format!(
                                "Expected tuple with {} elements, got {}",
                                EXPECTED_LEN,
                                tuple.len()
                            )));
                        }

                        // Reverse the tuple members to pop in correct order
                        tuple.reverse();

                        Ok(($(
                            $member::deserialize(tuple.pop().unwrap())?
                        ),+,))
                    }
                    _ => Err(Error::new("Expected tuple")),
                }
            }
        }
    };
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
            _ => Err(Error::new("Expected bool")),
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

                if number < f32::MIN as f64 || number > f32::MAX as f64 {
                    return Err(Error::new("f64 value out of range for f32"));
                }

                Ok(number as f32)
            }
            Value::Number(number) => {
                // Convert integers to f32
                let n = match number {
                    Number::U8(n) => n as f32,
                    Number::I8(n) => n as f32,
                    Number::U16(n) => n as f32,
                    Number::I16(n) => n as f32,
                    Number::U32(n) => n as f32,
                    Number::I32(n) => n as f32,
                    Number::U64(n) => n as f32,
                    Number::I64(n) => n as f32,
                    Number::U128(n) => n as f32,
                    Number::I128(n) => n as f32,
                    _ => unreachable!(),
                };

                Ok(n)
            }
            _ => Err(Error::new("Expected number for f32")),
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
                    Number::U64(n) => n as f64,
                    Number::I64(n) => n as f64,
                    Number::U128(n) => n as f64,
                    Number::I128(n) => n as f64,
                    _ => unreachable!(),
                };

                Ok(n)
            }
            _ => Err(Error::new("Expected number for f64")),
        }
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
            _ => Err(Error::new("Expected string")),
        }
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
            _ => Err(Error::new("Expected array")),
        }
    }
}

// -------------------------------- Tuple --------------------------------- //

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
            _ => Err(Error::new("Expected object")),
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
            _ => Err(Error::new("Expected object")),
        }
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
                let mut fields = $crate::internal::sys::BTreeMap::new();

                $(
                    fields.insert(
                        stringify!($field_name).to_string(),
                        self.$field_name.serialize()?
                    );
                )*

                Ok($crate::internal::Value::Struct(fields))
            }
        }

        impl $crate::Deserialize for $name {
            fn deserialize(value: $crate::internal::Value) -> $crate::internal::Result<Self> {
                match value {
                    $crate::internal::Value::Struct(fields) => {
                        $(
                            let $field_name =
                                match fields.get(stringify!($field_name)) {
                                    Some(value) =>
                                        <$field_type>::deserialize(value.clone())?,
                                    None => return Err(
                                        $crate::internal::Error::new(format!(
                                            "Missing `{}` field",
                                            stringify!($field_name)
                                        ))
                                    ),
                            };
                        )*

                        Ok($name {
                            $(
                                $field_name
                            ),*
                        })
                    }
                    _ => Err($crate::internal::Error::new(
                        format!("Expected struct for {}", stringify!($name))
                    ))
                }
            }
        }
    };
}
