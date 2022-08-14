#![allow(clippy::upper_case_acronyms)]

use nom::{
    branch::alt,
    bytes::complete::{is_a, is_not, tag, take_while, take_while1},
    character::complete::{alpha1, line_ending, satisfy, space0},
    combinator::{eof, map, map_res, opt, peek, value},
    sequence::{delimited, preceded, terminated, tuple},
    AsChar,
};
use nom_supreme::{
    error::ErrorTree, final_parser::final_parser, multi::collect_separated_terminated,
};

use crate::symbols::SymbolTable;

type IResult<I, O> = Result<(I, O), nom::Err<ErrorTree<I>>>;

pub fn parse(input: &str) -> Result<Vec<Line>, ErrorTree<&str>> {
    let p = collect_separated_terminated(Line::parse, line_ending, eof);
    final_parser(p)(input)
}

#[derive(Debug)]
pub enum Line {
    Label(Label),
    A(AInstruction),
    C(CInstruction),
    Comment,
    Empty,
}

impl Line {
    fn parse(input: &str) -> IResult<&str, Self> {
        let label_line = map(Label::parse, Line::Label);
        let a_inst_line = map(AInstruction::parse, Line::A);
        let c_inst_line = map(CInstruction::parse, Line::C);
        let comment_line = map(Self::parse_comment, |_| Line::Comment);
        let empty_line = map(Self::parse_whitespace, |_| Line::Empty);
        alt((
            terminated(label_line, opt(Self::parse_comment)),
            terminated(a_inst_line, opt(Self::parse_comment)),
            terminated(c_inst_line, opt(Self::parse_comment)),
            comment_line,
            empty_line,
        ))(input)
    }

    fn parse_comment(input: &str) -> IResult<&str, ()> {
        value((), preceded(space0, preceded(tag("//"), is_not("\r\n"))))(input)
    }

    fn parse_whitespace(input: &str) -> IResult<&str, ()> {
        value((), space0)(input)
    }
}

#[derive(Debug)]
pub struct Label {
    pub symbol: String,
}

impl Label {
    fn new(symbol: String) -> Self {
        Self { symbol }
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        let l = delimited(tag("("), parse_symbol, tag(")"));
        map(delimited(space0, l, space0), Self::new)(input)
    }
}

#[derive(Debug)]
pub enum Instruction {
    A(AInstruction),
    C(CInstruction),
}

impl Instruction {
    fn end_delim(input: &str) -> IResult<&str, ()> {
        value((), alt((tag("//"), line_ending, eof)))(input)
    }
}

#[derive(Debug)]
pub enum SymbolOrAddress {
    Symbol(String),
    Address(u16),
}

#[derive(Debug)]
pub struct AInstruction {
    symbol_or_address: SymbolOrAddress,
}

impl AInstruction {
    fn new(symbol_or_address: SymbolOrAddress) -> Self {
        Self { symbol_or_address }
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        let symbol_or_address = alt((
            map(parse_symbol, SymbolOrAddress::Symbol),
            map(parse_address, SymbolOrAddress::Address),
        ));
        let a = preceded(tag("@"), symbol_or_address);
        map(delimited(space0, a, space0), AInstruction::new)(input)
    }

    pub fn to_binary(&self, symbol_table: &mut SymbolTable) -> String {
        let address = match &self.symbol_or_address {
            SymbolOrAddress::Symbol(symbol) => symbol_table.resolve_variable(symbol),
            SymbolOrAddress::Address(a) => *a,
        };
        format!("{address:016b}")
    }
}

#[derive(Debug)]
pub struct CInstruction {
    destination: &'static str,
    computation: &'static str,
    jump: &'static str,
}

impl CInstruction {
    pub fn to_binary(&self) -> String {
        ["111", self.computation, self.destination, self.jump].concat()
    }

    fn parse(input: &str) -> IResult<&str, Self> {
        map(
            tuple((
                opt(Self::parse_dest),
                Self::parse_comp,
                opt(Self::parse_jump),
            )),
            |(dest_opt, computation, jump_opt)| {
                let destination = if let Some(dest) = dest_opt {
                    dest
                } else {
                    "000"
                };
                let jump = if let Some(jump) = jump_opt {
                    jump
                } else {
                    "000"
                };
                Self {
                    destination,
                    computation,
                    jump,
                }
            },
        )(input)
    }

    fn parse_dest(input: &str) -> IResult<&str, &'static str> {
        let ident = terminated(delimited(space0, alpha1, space0), tag("="));
        map_res(ident, |ident| match ident {
            "M" => Ok("001"),
            "D" => Ok("010"),
            "DM" | "MD" => Ok("011"),
            "A" => Ok("100"),
            "AM" | "MA" => Ok("101"),
            "AD" | "DA" => Ok("110"),
            "ADM" | "AMD" | "DMA" | "DAM" | "MAD" | "MDA" => Ok("111"),
            _ => Err(format!("Invalid destination: {ident}")),
        })(input)
    }

    fn parse_jump(input: &str) -> IResult<&str, &'static str> {
        let jump = delimited(space0, alpha1, space0);
        map_res(jump, |jump| match jump {
            "JGT" => Ok("001"),
            "JEQ" => Ok("010"),
            "JGE" => Ok("011"),
            "JLT" => Ok("100"),
            "JNE" => Ok("101"),
            "JLE" => Ok("110"),
            "JMP" => Ok("111"),
            _ => Err(format!("Invalid jump: {jump}")),
        })(input)
    }

    fn parse_comp(input: &str) -> IResult<&str, &'static str> {
        let comp = delimited(space0, is_a("01-+!ADM|&"), space0);
        let termination = alt((value((), tag(";")), peek(Instruction::end_delim)));
        let comp_term = terminated(comp, termination);
        map_res(comp_term, |comp: &str| match comp.trim() {
            "0" => Ok("0101010"),
            "1" => Ok("0111111"),
            "-1" => Ok("0111010"),
            "D" => Ok("0001100"),
            "A" => Ok("0110000"),
            "M" => Ok("1110000"),
            "!D" => Ok("0001101"),
            "!A" => Ok("0110001"),
            "!M" => Ok("1110001"),
            "-D" => Ok("0001111"),
            "-A" => Ok("0110011"),
            "-M" => Ok("1110011"),
            "D+1" => Ok("0011111"),
            "A+1" => Ok("0110111"),
            "M+1" => Ok("1110111"),
            "D-1" => Ok("0001110"),
            "A-1" => Ok("0110010"),
            "M-1" => Ok("1110010"),
            "D+A" => Ok("0000010"),
            "D+M" => Ok("1000010"),
            "D-A" => Ok("0010011"),
            "D-M" => Ok("1010011"),
            "A-D" => Ok("0000111"),
            "M-D" => Ok("1000111"),
            "D&A" => Ok("0000000"),
            "D&M" => Ok("1000000"),
            "D|A" => Ok("0010101"),
            "D|M" => Ok("1010101"),
            _ => Err(format!("Invalid comp: {comp}")),
        })(input)
    }
}

fn parse_symbol(input: &str) -> IResult<&str, String> {
    let valid_first_char = |c: char| c.is_alpha() || c == '_' || c == '.' || c == '$' || c == ':';
    let valid_tail_char = |c: char| valid_first_char(c) || c.is_ascii_digit();

    let parser = tuple((satisfy(valid_first_char), take_while(valid_tail_char)));
    let mut mapped = map(parser, |(head, tail)| format!("{head}{tail}"));
    mapped(input)
}

fn parse_address(input: &str) -> IResult<&str, u16> {
    map_res(
        take_while1(|c: char| c.is_ascii_digit()),
        |address: &str| address.parse::<u16>(),
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_label() {
        let res = Label::parse("(hi_YO)");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, label) = res.unwrap();
        assert_eq!("", remaining);
        assert_eq!("hi_YO", label.symbol);
    }

    #[test]
    fn parse_c_dest() {
        let res = CInstruction::parse_dest("D=");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, dest) = res.unwrap();
        assert_eq!("", remaining);
        assert_eq!("010", dest);
    }

    #[test]
    fn parse_c_comp() {
        let res = CInstruction::parse_comp("D;");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, comp) = res.unwrap();
        assert_eq!("", remaining);
        assert_eq!("0001100", comp);

        let res = CInstruction::parse_comp("D ");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, comp) = res.unwrap();
        assert_eq!("", remaining);
        assert_eq!("0001100", comp);
    }

    #[test]
    fn parse_c_jump() {
        let res = CInstruction::parse_jump("JMP\n");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, jump) = res.unwrap();
        assert_eq!("\n", remaining);
        assert_eq!("111", jump);
    }

    #[test]
    fn parse_c_inst_full() {
        let res = CInstruction::parse("D=D+1;JMP");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, c_inst) = res.unwrap();
        assert_eq!("", remaining);
        let bits = c_inst.to_binary();
        assert_eq!("1110011111010111", &bits);

        let res = CInstruction::parse("D = D+1 ; JMP ");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, c_inst) = res.unwrap();
        assert_eq!("", remaining);
        let bits = c_inst.to_binary();
        assert_eq!("1110011111010111", &bits);

        let res = CInstruction::parse("D=M ");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, c_inst) = res.unwrap();
        assert_eq!("", remaining);
        let bits = c_inst.to_binary();
        assert_eq!("1111110000010000", &bits);
    }

    #[test]
    fn parse_c_inst_no_jump_or_dest() {
        let res = CInstruction::parse("D=D+1");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, c_inst) = res.unwrap();
        assert_eq!("", remaining);
        let bits = c_inst.to_binary();
        assert_eq!("1110011111010000", &bits);

        let res = CInstruction::parse("D+1");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, c_inst) = res.unwrap();
        assert_eq!("", remaining);
        let bits = c_inst.to_binary();
        assert_eq!("1110011111000000", &bits);

        let res = CInstruction::parse("D+1;JMP");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, c_inst) = res.unwrap();
        assert_eq!("", remaining);
        let bits = c_inst.to_binary();
        assert_eq!("1110011111000111", &bits);
    }

    #[test]
    fn parse_a_inst() {
        let mut symbol_table = SymbolTable::init_symbol_table();
        let res = AInstruction::parse("@f");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, a_inst) = res.unwrap();
        assert_eq!("", remaining);
        let bits = a_inst.to_binary(&mut symbol_table);
        assert_eq!("0000000000010000", &bits);
    }

    #[test]
    fn parse_empty_line() {
        let res = Line::parse("\n");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, line) = res.unwrap();
        assert_eq!("\n", remaining);
        match line {
            Line::Empty => {}
            x => panic!("expected empty line parsed, got: {x:?}"),
        }

        let res = Line::parse("");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, line) = res.unwrap();
        assert_eq!("", remaining);
        match line {
            Line::Empty => {}
            x => panic!("expected empty line parsed, got: {x:?}"),
        }
    }

    #[test]
    fn parse_line() {
        let res = Line::parse("D=D+1;JMP\n");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, line) = res.unwrap();
        assert_eq!("\n", remaining);
        match line {
            Line::C(c) => {
                let bits = c.to_binary();
                assert_eq!("1110011111010111", &bits);
            }
            x => panic!("misparsed line: {x:?}"),
        }

        let res = Line::parse("MD=M+1\r\n");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, line) = res.unwrap();
        assert_eq!("\r\n", remaining);
        match line {
            Line::C(c) => {
                let bits = c.to_binary();
                assert_eq!("1111110111011000", &bits);
            }
            x => panic!("misparsed line: {x:?}"),
        }
    }

    #[test]
    fn parse_line_trailing_comment() {
        let res = Line::parse("D=M //comment  \n");
        assert!(res.is_ok(), "{res:?}");
        let (remaining, line) = res.unwrap();
        assert_eq!("\n", remaining);
        match line {
            Line::C(c) => {
                let bits = c.to_binary();
                assert_eq!("1111110000010000", &bits);
            }
            x => panic!("misparsed line: {x:?}"),
        }
    }

    #[test]
    fn parse_lines() {
        let res = parse("D=D+1;JMP\n\n@a\n");
        assert!(res.is_ok(), "{res:?}");
        let lines = res.unwrap();
        assert_eq!(lines.len(), 4);
    }

    #[test]
    fn error_lines() {
        let res = parse("D=D+1;JMP\na\n@a\n");
        assert!(res.is_err(), "{res:?}");
    }
}
