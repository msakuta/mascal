use std::{
    collections::HashMap,
    io::{Read, Write},
};

use crate::ReadError;

use super::{read_str, read_usize, write_str, write_usize, Bytecode};

impl Bytecode {
    pub(super) fn write_debug_info(
        debug: &DebugInfo,
        writer: &mut impl Write,
    ) -> std::io::Result<()> {
        write_str(&debug.file_name, writer)?;
        write_usize(debug.source_map.len(), writer)?;
        for (fname, debug) in debug.source_map.iter() {
            write_str(fname, writer)?;
            writer.write_all(&debug.len().to_le_bytes())?;
            for line_info in debug {
                line_info.serialize(writer)?;
            }
        }
        Ok(())
    }

    pub(super) fn read_debug_info(reader: &mut impl Read) -> Result<DebugInfo, ReadError> {
        let file_name = read_str(reader)?;
        let len = read_usize(reader)?;
        let source_map = (0..len)
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
        Ok(DebugInfo {
            file_name,
            source_map,
        })
    }
}

#[derive(Debug)]
pub struct DebugInfo {
    file_name: String,
    source_map: HashMap<String, Vec<LineInfo>>,
}

impl DebugInfo {
    pub fn new(source_map: HashMap<String, Vec<LineInfo>>) -> Self {
        Self {
            file_name: "".to_string(),
            source_map,
        }
    }

    pub fn get(&self, fname: &str) -> Option<&Vec<LineInfo>> {
        self.source_map.get(fname)
    }

    pub fn file_name(&self) -> &str {
        &self.file_name
    }

    pub(super) fn set_file_name(&mut self, file_name: impl Into<String>) {
        self.file_name = file_name.into()
    }
}

#[derive(Debug, Clone, Copy)]
/// Source mapping between line number and bytecode bytes.
/// u32 should suffice since no one would write more than 4 billion bytes of source codes.
pub struct LineInfo {
    pub instruction: u32,
    pub src_line: u32,
    pub src_column: u32,
    pub src_len: u32,
}

impl LineInfo {
    pub(crate) fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        writer.write_all(&self.instruction.to_le_bytes())?;
        writer.write_all(&self.src_line.to_le_bytes())?;
        writer.write_all(&self.src_column.to_le_bytes())?;
        writer.write_all(&self.src_len.to_le_bytes())?;
        Ok(())
    }

    pub(crate) fn deserialize(reader: &mut impl Read) -> std::io::Result<Self> {
        let mut buf = [0u8; std::mem::size_of::<u32>()];
        reader.read_exact(&mut buf)?;
        let instruction = u32::from_le_bytes(buf);
        reader.read_exact(&mut buf)?;
        let src_line = u32::from_le_bytes(buf);
        reader.read_exact(&mut buf)?;
        let src_column = u32::from_le_bytes(buf);
        reader.read_exact(&mut buf)?;
        let src_len = u32::from_le_bytes(buf);
        Ok(Self {
            instruction,
            src_line,
            src_column,
            src_len,
        })
    }
}
