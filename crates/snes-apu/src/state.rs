//! Checked little-endian serialization of our audio state; no native layout,
//! raw pointers, unchecked indexing or unbounded container lengths on import.
use std::collections::VecDeque;
pub(crate) struct Reader<'a>(pub &'a [u8]);
impl<'a> Reader<'a> {
    pub fn take(&mut self, size: usize) -> Result<&'a [u8], &'static str> {
        if size > self.0.len() {
            return Err("Truncated SPC state");
        }
        let (data, rest) = self.0.split_at(size);
        self.0 = rest;
        Ok(data)
    }
}
pub(crate) trait State: Sized {
    fn encode(&self, out: &mut Vec<u8>);
    fn decode(input: &mut Reader<'_>) -> Result<Self, &'static str>;
}
macro_rules! integer {
    ($($ty:ty),*) => { $(impl State for $ty {
        fn encode(&self,out:&mut Vec<u8>) { out.extend_from_slice(&self.to_le_bytes()); }
        fn decode(input:&mut Reader<'_>) -> Result<Self,&'static str> {
            Ok(Self::from_le_bytes(input.take(std::mem::size_of::<Self>())?.try_into().unwrap()))
        }
    })* };
}
integer!(u8, u16, u32, i16, i32);
impl State for bool {
    fn encode(&self, out: &mut Vec<u8>) {
        out.push(u8::from(*self));
    }
    fn decode(input: &mut Reader<'_>) -> Result<Self, &'static str> {
        match u8::decode(input)? {
            0 => Ok(false),
            1 => Ok(true),
            _ => Err("Invalid SPC state flag"),
        }
    }
}
impl<T: State, const N: usize> State for [T; N] {
    fn encode(&self, out: &mut Vec<u8>) {
        for value in self {
            value.encode(out);
        }
    }
    fn decode(input: &mut Reader<'_>) -> Result<Self, &'static str> {
        let values: Vec<T> = (0..N).map(|_| T::decode(input)).collect::<Result<_, _>>()?;
        values.try_into().map_err(|_| "Invalid SPC array length")
    }
}
impl<T: State> State for VecDeque<T> {
    fn encode(&self, out: &mut Vec<u8>) {
        (self.len() as u16).encode(out);
        for value in self {
            value.encode(out);
        }
    }
    fn decode(input: &mut Reader<'_>) -> Result<Self, &'static str> {
        let size = u16::decode(input)?;
        if size > 8192 {
            return Err("SPC sample queue exceeds its bound");
        }
        (0..size).map(|_| T::decode(input)).collect()
    }
}
impl State for crate::SpcRam {
    fn encode(&self, out: &mut Vec<u8>) {
        out.extend_from_slice(self.bytes());
    }
    fn decode(input: &mut Reader<'_>) -> Result<Self, &'static str> {
        Self::from_bytes(input.take(crate::RAM_BYTES)?)
    }
}
macro_rules! snapshot {
    ($ty:ty,$($field:ident),+ $(,)?) => { impl crate::state::State for $ty {
        fn encode(&self,out:&mut Vec<u8>) { $(crate::state::State::encode(&self.$field,out);)+ }
        fn decode(input:&mut crate::state::Reader<'_>) -> Result<Self,&'static str> {
            Ok(Self { $($field:crate::state::State::decode(input)?,)+ })
        }
    } };
}
pub(crate) use snapshot;
