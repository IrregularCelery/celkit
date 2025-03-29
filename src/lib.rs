pub trait Serialize {
    fn serialize(&self);
}

pub trait Deserialize {
    fn deserialize(&self);
}
