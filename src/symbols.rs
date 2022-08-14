use std::collections::HashMap;

#[derive(Debug)]
pub struct SymbolTable {
    n_vars: usize,
    symbols: HashMap<String, u16>,
}

impl SymbolTable {
    /// Allocate fresh symbol table with only pre-defined symbols
    pub fn init_symbol_table() -> Self {
        let n_vars = 0;
        let symbols = [
            ("SP", 0),
            ("LCL", 1),
            ("ARG", 2),
            ("THIS", 3),
            ("THAT", 4),
            ("R0", 0),
            ("R1", 1),
            ("R2", 2),
            ("R3", 3),
            ("R4", 4),
            ("R5", 5),
            ("R6", 6),
            ("R7", 7),
            ("R8", 8),
            ("R9", 9),
            ("R10", 10),
            ("R11", 11),
            ("R12", 12),
            ("R13", 13),
            ("R14", 14),
            ("R15", 15),
            ("SCREEN", 16384),
            ("KBD", 24576),
        ]
        .into_iter()
        .map(|(symbol, i)| (symbol.to_string(), i))
        .collect();
        Self { n_vars, symbols }
    }

    /// Add given label to symbol table
    /// If label was already added returns previous line number for it
    pub fn add_label(&mut self, label: String, line_nr: usize) -> Option<u16> {
        self.symbols.insert(label, line_nr as u16)
    }

    /// Resolves given variable by creating new memory address for its
    /// or returning already previously allocated address
    pub fn resolve_variable(&mut self, variable: &str) -> u16 {
        match self.symbols.get(variable) {
            Some(v) => *v,
            None => {
                self.n_vars += 1;
                let mem_address = (self.n_vars + 15) as u16;
                self.symbols.insert(variable.to_string(), mem_address);
                mem_address
            }
        }
    }
}
