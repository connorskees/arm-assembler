use std::{
    collections::BTreeSet,
    fmt::{self, Write},
};

#[derive(Debug, Clone)]
pub struct Case<'a> {
    pub(crate) clauses: Vec<CaseClause<'a>>,
    pub(crate) when: Vec<When<'a>>,
}

#[derive(Debug, Clone)]
pub enum CaseClause<'a> {
    BitAccess(BitAccess),
    Field(&'a str),
}

#[derive(Debug, Clone)]
pub struct When<'a> {
    pub(crate) clauses: Vec<WhenClause<'a>>,
    pub(crate) body: WhenBody<'a>,
}

#[derive(Debug, Clone)]
pub enum WhenBody<'a> {
    Case {
        fields: Vec<Field<'a>>,
        case: Box<Case<'a>>,
    },
    Encoding {
        name: &'a str,
        mnemonic: &'a str,
    },
    Unallocated,
    Unpredictable,
}

#[derive(Debug, Clone)]
pub enum WhenClause<'a> {
    Any,
    Concrete(&'a str),
    Not(&'a str),
}

#[derive(Debug, Clone)]
pub struct Field<'a> {
    pub(crate) name: &'a str,
    pub(crate) bit_access: BitAccess,
}

#[derive(Debug, Clone, Copy)]
pub struct BitAccess {
    pub(crate) start: u8,
    pub(crate) len: u8,
}

impl fmt::Display for BitAccess {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.start == 0 {
            write!(f, "inst & 0b{}", "1".repeat(self.len as usize))
        } else {
            write!(
                f,
                "(inst >> {}) & 0b{}",
                self.start,
                "1".repeat(self.len as usize)
            )
        }
    }
}

#[cfg(test)]
mod test {
    use super::BitOptions;

    #[test]
    fn bit_options() {
        assert_eq!(
            BitOptions("'1xx1'").options(),
            vec![
                "0b1001".to_owned(),
                "0b1101".to_owned(),
                "0b1011".to_owned(),
                "0b1111".to_owned()
            ]
        );

        assert_eq!(
            BitOptions("'1x'").options(),
            vec!["0b10".to_owned(), "0b11".to_owned(),]
        );

        assert_eq!(
            BitOptions("'x1'").options(),
            vec!["0b01".to_owned(), "0b11".to_owned(),]
        );
        // 1xx1
        // 1x1x
    }
}

struct BitOptions<'a>(&'a str);

impl<'a> BitOptions<'a> {
    pub fn options(&self) -> Vec<String> {
        // let mut buffer = String::new();

        // "01xx10x".split('x').collect::<Vec<_>>()

        // let number_of_opts = self.0.bytes().filter(|&c| c == b'x').count();

        let mut opts = Vec::new(); //vec![String::new(); number_of_opts];

        for part in self.0.trim_matches('\'').chars() {
            if opts.is_empty() {
                if part == 'x' {
                    opts.push("0b0".to_owned());
                    opts.push("0b1".to_owned());
                } else {
                    opts.push(format!("0b{}", part));
                }

                continue;
            }

            if part != 'x' {
                for opt in &mut opts {
                    opt.push(part);
                }

                continue;
            }

            let mut with_zero = opts
                .clone()
                .into_iter()
                .map(|mut o| {
                    o.push('0');
                    o
                })
                .collect::<Vec<_>>();
            let mut with_one = opts
                .into_iter()
                .map(|mut o| {
                    o.push('1');
                    o
                })
                .collect::<Vec<_>>();

            with_zero.append(&mut with_one);

            opts = with_zero;
        }

        opts.into_iter()
            .map(|o| {
                if o.len() == 3 {
                    o.trim_start_matches("0b").to_owned()
                } else {
                    o
                }
            })
            .collect()
    }
}

struct Invert<'a> {
    var_name: String,
    bit_options: BitOptions<'a>,
}

impl<'a> fmt::Display for Invert<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let checks = self
            .bit_options
            .options()
            .into_iter()
            .map(|b| format!("{} != {b}", self.var_name))
            .collect::<Vec<_>>()
            .join(" && ");

        write!(f, "{}", checks)
    }
}

pub struct CaseVisitor {}

impl<'a> CaseVisitor {
    fn write_case_with_fields(
        case: Case<'a>,
        fields: &[Field],
        indentation_amt: usize,
        buffer: &mut String,
        opcodes: &mut BTreeSet<&'a str>,
        encodings: &mut BTreeSet<&'a str>,
    ) -> anyhow::Result<()> {
        let indentation = vec![" "; indentation_amt].join("");

        writeln!(buffer, "{{")?;

        for field in fields {
            writeln!(
                buffer,
                "{indentation}    let {} = {};",
                field.name, field.bit_access,
            )?;
        }

        write!(
            buffer,
            "{}",
            CaseVisitor::write(case, indentation_amt + 4, true, opcodes, encodings)?
        )?;

        write!(buffer, "\n{indentation}}}")?;

        Ok(())
    }

    pub fn write(
        case: Case<'a>,
        indentation_amt: usize,
        in_block: bool,
        opcodes: &mut BTreeSet<&'a str>,
        encodings: &mut BTreeSet<&'a str>,
    ) -> anyhow::Result<String> {
        let mut buffer = String::new();

        let indentation = vec![" "; indentation_amt].join("");

        if case.clauses.is_empty() {
            let end_char = if in_block { ';' } else { ',' };
            assert_eq!(case.when.len(), 1);

            let when = &case.when[0];

            assert!(when.clauses.is_empty());

            match &when.body {
                WhenBody::Case { .. } => panic!(),
                WhenBody::Encoding { mnemonic, name } => {
                    opcodes.insert(mnemonic);
                    encodings.insert(name);
                    write!(buffer, "return OpCode::{}{end_char}", mnemonic)?
                }
                WhenBody::Unallocated => write!(buffer, "return OpCode::Unallocated{end_char}")?,
                WhenBody::Unpredictable => {
                    write!(buffer, "return OpCode::Unpredictable{end_char}")?
                }
            };

            return Ok(buffer);
        }

        let end_char = if in_block { ',' } else { ',' };

        if in_block {
            write!(buffer, "{indentation}")?;
        }

        let mut clauses = Vec::new();

        for clause in &case.clauses {
            match clause {
                CaseClause::BitAccess(bit_access) => clauses.push(bit_access.to_string()),
                &CaseClause::Field(field) => clauses.push(field.to_owned()),
            }
        }

        if clauses.len() == 1 {
            write!(buffer, "match {} ", clauses[0])?;
        } else {
            write!(buffer, "match ({}) ", clauses.join(", "))?;
        }

        writeln!(buffer, "{{")?;

        for when in &case.when {
            write!(buffer, "{indentation}    ")?;

            let mut nots = Vec::new();

            let mut match_elements = Vec::new();

            for clause in &when.clauses {
                match clause {
                    WhenClause::Any => match_elements.push("_".to_owned()),
                    WhenClause::Concrete(opts) => {
                        let options = BitOptions(opts).options().join(" | ");

                        match_elements.push(options);

                        // write!(buffer, "{}, ", options)?;
                    }
                    WhenClause::Not(opts) => {
                        let bit_options = BitOptions(opts);

                        let var_name = format!("var{}", nots.len());

                        match_elements.push(var_name.clone());

                        nots.push(Invert {
                            bit_options,
                            var_name,
                        });
                    }
                }
            }

            if match_elements.len() == 1 {
                write!(buffer, "{}", match_elements[0])?;
            } else {
                write!(buffer, "({})", match_elements.join(", "))?;
            }

            if nots.is_empty() {
                write!(buffer, " => ")?;
            } else {
                write!(
                    buffer,
                    " if {} => ",
                    nots.into_iter()
                        .map(|n| n.to_string())
                        .collect::<Vec<_>>()
                        .join(" && ")
                )?;
            }

            match &when.body {
                WhenBody::Case { case, fields } => {
                    let case = (**case).clone();

                    if fields.is_empty() {
                        write!(
                            buffer,
                            "{}",
                            CaseVisitor::write(
                                case,
                                indentation_amt + 4,
                                false,
                                opcodes,
                                encodings
                            )?
                        )?;
                    } else {
                        Self::write_case_with_fields(
                            case,
                            fields,
                            indentation_amt + 4,
                            &mut buffer,
                            opcodes,
                            encodings,
                        )?;
                    }
                }
                WhenBody::Encoding { mnemonic, .. } => {
                    opcodes.insert(mnemonic);
                    write!(buffer, "return OpCode::{mnemonic}{end_char}")?
                }
                WhenBody::Unallocated => write!(buffer, "return OpCode::Unallocated{end_char}")?,
                WhenBody::Unpredictable => {
                    write!(buffer, "return OpCode::Unpredictable{end_char}")?
                }
            }

            writeln!(buffer)?;
        }

        writeln!(
            buffer,
            "{indentation}    _ => return OpCode::Unpredictable,"
        )?;

        write!(buffer, "{indentation}}}")?;

        Ok(buffer)
    }
}
