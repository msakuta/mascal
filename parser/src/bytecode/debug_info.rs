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
        debug.serialize(writer)
    }

    pub(super) fn read_debug_info(reader: &mut impl Read) -> Result<DebugInfo, ReadError> {
        DebugInfo::deserialize(reader)
    }
}

#[derive(Debug)]
pub struct DebugInfo {
    file_name: String,
    source_map: HashMap<String, FunctionInfo>,
}

impl DebugInfo {
    pub fn new(source_map: HashMap<String, FunctionInfo>) -> Self {
        Self {
            file_name: "".to_string(),
            source_map,
        }
    }

    pub fn get(&self, fname: &str) -> Option<&FunctionInfo> {
        self.source_map.get(fname)
    }

    pub fn file_name(&self) -> &str {
        &self.file_name
    }

    pub(super) fn set_file_name(&mut self, file_name: impl Into<String>) {
        self.file_name = file_name.into()
    }

    fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        write_str(&self.file_name, writer)?;
        write_usize(self.source_map.len(), writer)?;
        for (fname, fun) in self.source_map.iter() {
            write_str(fname, writer)?;
            fun.serialize(writer)?;
        }
        Ok(())
    }

    fn deserialize(reader: &mut impl Read) -> Result<Self, ReadError> {
        let file_name = read_str(reader)?;
        let len = read_usize(reader)?;
        let source_map = (0..len)
            .map(|_| -> Result<_, ReadError> {
                let name = read_str(reader)?;
                let fn_info = FunctionInfo::deserialize(reader)?;
                Ok((name, fn_info))
            })
            .collect::<Result<HashMap<String, FunctionInfo>, _>>()?;
        Ok(DebugInfo {
            file_name,
            source_map,
        })
    }
}

#[derive(Debug)]
pub struct FunctionInfo {
    /// Mapping between variable name and stack index
    pub vars: HashMap<String, usize>,
    pub line_info: Vec<LineInfo>,
}

impl FunctionInfo {
    pub(crate) fn new(vars: HashMap<String, usize>, line_info: Vec<LineInfo>) -> Self {
        Self { vars, line_info }
    }

    fn serialize(&self, writer: &mut impl Write) -> std::io::Result<()> {
        write_usize(self.vars.len(), writer)?;
        for (var_name, var_idx) in &self.vars {
            write_str(var_name, writer)?;
            write_usize(*var_idx, writer)?;
        }
        write_usize(self.line_info.len(), writer)?;
        for line_info in &self.line_info {
            line_info.serialize(writer)?;
        }
        Ok(())
    }

    fn deserialize(reader: &mut impl Read) -> Result<Self, ReadError> {
        let vars_len = read_usize(reader)?;
        let vars = (0..vars_len)
            .map(|_| {
                let var_name = read_str(reader)?;
                let var_idx = read_usize(reader)?;
                Ok((var_name, var_idx))
            })
            .collect::<Result<HashMap<String, _>, ReadError>>()?;

        let lines_len = read_usize(reader)?;
        let line_info = (0..lines_len)
            .map(|_| LineInfo::deserialize(reader))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { vars, line_info })
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
