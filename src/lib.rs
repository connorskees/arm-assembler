use std::collections::{BTreeMap, BTreeSet, HashSet};
use std::fs::OpenOptions;
use std::io::Write;

pub use asl::ast::*;
pub use asl::parser::AslParser;
use asl::CaseVisitor;
use codegen::decode;
pub use register::*;

mod asl;
mod codegen;
mod register;

pub struct Inst {
    pub opcode: decode::OpCode,
    pub raw: u32,
}

pub fn decode_inst(bytes: u32) -> Inst {
    let opcode = decode::decode_inst(bytes);
    Inst { opcode, raw: bytes }
}

#[derive(Debug, Clone, Copy)]
pub enum Shift {
    Lsl = 0b00,
    Lsr = 0b01,
    Asr = 0b10,
    Ror = 0b11,
}

fn write_prelude(file: &mut dyn Write) -> anyhow::Result<()> {
    writeln!(
        file,
        "//! This file is autogenerated! Manual changes will not be saved."
    )?;
    writeln!(file, "#![allow(non_camel_case_types, unused_variables, dead_code, non_snake_case, unreachable_patterns)]")?;
    writeln!(file, "#![allow(clippy::all)]")?;

    Ok(())
}

#[allow(dead_code)]
fn codegen() -> anyhow::Result<()> {
    let arch_decode_str = std::fs::read("./src/asl/arch_decode.asl")?;
    let arch_instrs_str = std::fs::read("./src/asl/arch_instrs.asl")?;

    let arch_decode = asl::parser::AslParser::parse_toplevel_case(&arch_decode_str)?;
    let arch_instrs = asl::parser::AslParser::parse_instruction_file(&arch_instrs_str)?;

    dbg!(&arch_instrs);

    let mut opcode_to_fields = BTreeMap::new();

    for def in arch_instrs {
        for encoding in def.encodings {
            let mut fields = encoding.fields.clone();
            fields.reverse();
            opcode_to_fields.insert(encoding.name, fields);
        }
    }

    let mut opcodes = BTreeSet::new();
    let mut encodings = BTreeSet::new();

    let decode_codegen = CaseVisitor::write(arch_decode, 0, false, &mut opcodes, &mut encodings)?;

    let mut decode_file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("./src/decode.rs")?;

    let mut mnemonic_file = OpenOptions::new()
        .create(true)
        .write(true)
        .truncate(true)
        .open("./src/mnemonic.rs")?;

    write_prelude(&mut decode_file)?;
    write_prelude(&mut mnemonic_file)?;

    writeln!(decode_file, "#[derive(Debug)]")?;
    writeln!(decode_file, "pub enum OpCode {{")?;
    for code in &opcodes {
        writeln!(decode_file, "    {code},")?;
    }
    writeln!(decode_file, "    Unallocated,")?;
    writeln!(decode_file, "    Unpredictable,")?;
    writeln!(decode_file, "}}")?;

    writeln!(decode_file, "pub fn decode_inst(inst: u32) -> OpCode {{").unwrap();
    writeln!(decode_file, "{decode_codegen}\n}}").unwrap();

    for code in &opcodes {
        writeln!(
            decode_file,
            "#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]"
        )?;
        writeln!(decode_file, "pub struct {code}(u32);")?;
        let fields: &[_] = opcode_to_fields.get(code).map(Vec::as_slice).unwrap_or(&[]);

        if fields.is_empty() {
            continue;
        }

        writeln!(decode_file, "impl {code} {{")?;

        for field in fields {
            writeln!(decode_file, "    pub fn {}(self) -> u32 {{", field.name)?;
            writeln!(decode_file, "        let inst = self.0;")?;
            writeln!(decode_file, "        {}", field.bit_access)?;
            writeln!(decode_file, "    }}")?;
        }

        writeln!(decode_file, "}}\n")?;
    }

    let rust_keywords = HashSet::<&'static str>::from_iter([
        "as", "break", "const", "continue", "crate", "else", "enum", "extern", "false", "fn",
        "for", "if", "impl", "in", "let", "loop", "match", "mod", "move", "mut", "pub", "ref",
        "return", "self", "Self", "static", "struct", "super", "trait", "true", "type", "unsafe",
        "use", "where", "while", "async", "await", "dyn", "abstract", "become", "box", "do",
        "final", "macro", "override", "priv", "typeof", "unsized", "virtual", "yield", "try",
    ]);

    let mnemonics = BTreeSet::from_iter(opcodes.iter().map(|code| {
        let mut mnemonic = code.split_once('_').unwrap().0.to_ascii_lowercase();
        if rust_keywords.contains(mnemonic.as_str()) {
            mnemonic = format!("r#{mnemonic}");
        }
        mnemonic
    }));

    writeln!(mnemonic_file, "#[derive(Debug)]")?;
    writeln!(mnemonic_file, "pub enum Mnemonic {{")?;

    for mnemonic in &mnemonics {
        writeln!(mnemonic_file, "    {mnemonic},")?;
    }
    writeln!(mnemonic_file, "}}")?;

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{asl, codegen, decode::decode_inst};

    #[test]
    fn test_handwritten_inst_parser() {
        let input = std::fs::read("./src/asl/arch_instrs.asl").unwrap();

        let expr = asl::parser::AslParser::parse_instruction_file(&input).unwrap();

        dbg!(expr);
    }

    #[test]
    fn test_handwritten_parser() {
        codegen().unwrap();
        panic!();
    }

    #[test]
    #[ignore = "skipping decoding"]
    fn decodes_single_instructions() {
        // br	x3
        let inst = decode_inst(0b11010110000111110000000001100000_u32);
        dbg!(inst);

        // mov	w8, #19 (Movz32)
        let inst = decode_inst(0b0_10_100101_00_0000000000010011_01000_u32);
        dbg!(inst);

        // mov x0, #0 (Movz64)
        let inst = decode_inst(0b11010010100000000000000000000000_u32);
        dbg!(inst);

        // mov	x0, x22 (OrrShiftedRegister64)
        let inst = decode_inst(0b10101010000101100000001111100000_u32);
        dbg!(inst);

        // str	x9, [sp, #80] (StrImm64)
        let inst = decode_inst(0b11111001000000000010101111101001_u32);
        dbg!(inst);

        // ret
        let inst = decode_inst(0b11010110010111110000001111000000_u32);
        dbg!(inst);
        panic!();
    }
}
