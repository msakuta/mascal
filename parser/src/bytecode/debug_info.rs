use std::{
    collections::HashMap,
    io::{Read, Write},
};

use crate::ReadError;

use super::{read_str, write_str, Bytecode};

impl Bytecode {
    pub(super) fn write_debug_info(
        debug: &DebugInfo,
        writer: &mut impl Write,
    ) -> std::io::Result<()> {
        writer.write_all(&debug.len().to_le_bytes())?;
        for (fname, debug) in debug.iter() {
            write_str(fname, writer)?;
            writer.write_all(&debug.len().to_le_bytes())?;
            for line_info in debug {
                line_info.serialize(writer)?;
            }
        }
        Ok(())
    }

    pub(super) fn read_debug_info(
        reader: &mut impl Read,
    ) -> Result<HashMap<String, Vec<LineInfo>>, ReadError> {
        println!("Reading DEBUG tag");
        let mut len = [0u8; std::mem::size_of::<usize>()];
        reader.read_exact(&mut len)?;
        let len = usize::from_le_bytes(len);
        println!("Reading {len} debug info");
        let debug = (0..len)
            .map(|_| -> Result<_, ReadError> {
                let name = read_str(reader)?;
                let mut buf = [0u8; std::mem::size_of::<usize>()];
                reader.read_exact(&mut buf)?;
                let len = usize::from_le_bytes(buf);
                let line_info = (0..len)
                    .map(|_| LineInfo::deserialize(reader))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok((name, line_info))
            })
            .collect::<Result<HashMap<String, Vec<LineInfo>>, _>>()?;
        println!("Debug: {debug:?}");
        Ok(debug)
    }
}

pub type DebugInfo = HashMap<String, Vec<LineInfo>>;

#[derive(Debug)]
/// Source mapping between line number and bytecode bytes.
/// u32 should suffice since no one would write more than 4 billion bytes of source codes.
pub struct LineInfo {
    pub src_start: u32,
    pub src_end: u32,
    pub byte_start: u32,
    pub byte_end: u32,
}

impl LineInfo {
    pub(crate) fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        writer.write_all(&self.src_start.to_le_bytes())?;
        writer.write_all(&self.src_end.to_le_bytes())?;
        writer.write_all(&self.byte_start.to_le_bytes())?;
        writer.write_all(&self.byte_end.to_le_bytes())?;
        Ok(())
    }

    pub(crate) fn deserialize(reader: &mut impl Read) -> std::io::Result<Self> {
        let mut buf = [0u8; std::mem::size_of::<u32>()];
        reader.read_exact(&mut buf)?;
        let src_start = u32::from_le_bytes(buf);
        reader.read_exact(&mut buf)?;
        let src_end = u32::from_le_bytes(buf);
        reader.read_exact(&mut buf)?;
        let byte_start = u32::from_le_bytes(buf);
        reader.read_exact(&mut buf)?;
        let byte_end = u32::from_le_bytes(buf);
        Ok(Self {
            src_start,
            src_end,
            byte_start,
            byte_end,
        })
    }
}
