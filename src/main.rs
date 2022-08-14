#![allow(clippy::upper_case_acronyms)]

use std::{env, fs};

use crate::{
    parsing::{parse, Instruction, Line},
    symbols::SymbolTable,
};

mod parsing;
mod symbols;

fn asm_to_bin(asm_src: &str) -> String {
    // parse source file into typed lines
    let lines = parse(asm_src).expect("parse failure");
    let mut symbol_table = SymbolTable::init_symbol_table();

    // gather instructions, discard comments/empty lines
    // and add all labels to symbol table
    let mut instructions = Vec::new();
    for (i, line) in lines.into_iter().enumerate() {
        match line {
            Line::Label(l) => {
                let next_instruction_index = instructions.len();
                let v = symbol_table.add_label(l.symbol, next_instruction_index);
                if let Some(l) = v {
                    panic!("[Line {i}]: Label {l} already defined");
                }
            }
            Line::A(inst) => {
                instructions.push(Instruction::A(inst));
            }
            Line::C(inst) => {
                instructions.push(Instruction::C(inst));
            }
            _ => {}
        }
    }

    let mut res = String::new();
    // convert instructions to binary
    for inst in instructions {
        // println!("{inst:?}");
        match inst {
            // use symbol table to translate A instructions
            // modifies symbol table to include new variables
            Instruction::A(a) => res.push_str(&a.to_binary(&mut symbol_table)),
            Instruction::C(c) => res.push_str(&c.to_binary()),
        }
        res.push('\n');
    }
    res
}

fn main() {
    let mut args = env::args();
    args.next();
    let asm_file = args.next().unwrap();
    let asm_src = fs::read_to_string(asm_file).unwrap();
    let asm_bin = asm_to_bin(&asm_src);
    print!("{asm_bin}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const ADD_ASM: &str = include_str!("../test_resources/Add.asm");
    const ADD_BIN: &str = include_str!("../test_resources/Add.hack");
    const MAX_ASM: &str = include_str!("../test_resources/Max.asm");
    const MAX_BIN: &str = include_str!("../test_resources/Max.hack");
    const PONG_ASM: &str = include_str!("../test_resources/Pong.asm");
    const PONG_BIN: &str = include_str!("../test_resources/Pong.hack");
    const RECT_ASM: &str = include_str!("../test_resources/Rect.asm");
    const RECT_BIN: &str = include_str!("../test_resources/Rect.hack");

    #[test]
    fn parse_add_program() {
        let res = asm_to_bin(ADD_ASM);
        assert_eq!(res, ADD_BIN);
    }

    #[test]
    fn parse_max_program() {
        let res = asm_to_bin(MAX_ASM);
        assert_eq!(res, MAX_BIN);
    }

    #[test]
    fn parse_pong_program() {
        let res = asm_to_bin(PONG_ASM);
        assert_eq!(res, PONG_BIN);
    }

    #[test]
    fn parse_rect_program() {
        let res = asm_to_bin(RECT_ASM);
        assert_eq!(res, RECT_BIN);
    }
}
