#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum OpCode {
    // PC-rel. addressing
    Adr,
    Adrp,

    // Add/subtract (immediate)
    AddImm32,
    AddsImm32,
    SubImm32,
    SubsImm32,
    AddImm64,
    AddsImm64,
    SubImm64,
    SubsImm64,

    // Add/subtract (immediate, with tags)
    Addg,
    Subg,

    // Logical (immediate)
    AndImm32,
    OrrImm32,
    EorImm32,
    AndsImm32,
    AndImm64,
    OrrImm64,
    EorImm64,
    AndsImm64,

    // Move wide (immediate)
    Movn32,
    Movz32,
    Movk32,
    Movn64,
    Movz64,
    Movk64,

    // Bitfield
    Sbfm32,
    Bfm32,
    Ubfm32,
    Sbfm64,
    Bfm64,
    Ubfm64,

    // Extract
    Extr32,
    Extr64,

    // Unconditional branch (register)
    Br,
    Blr,
    Ret,
    Eret,
    Drps,

    // Unconditional branch (immediate)
    B,
    Bl,

    // Compare and branch (immediate)
    Cbz32,
    Cbnz32,
    Cbz64,
    Cbnz64,

    // Data-processing (2 source)
    Udiv32,
    Sdiv32,
    Lslv32,
    Lsrv32,
    Asrv32,
    Rorv32,
    Subp,
    Udiv64,
    Sdiv64,
    Irg,
    Gmi,
    Lslv64,
    Lsrv64,
    Asrv64,
    Rorv64,
    Pacga,
    Subps,

    // Data-processing (1 source)
    Rbit32,
    Rev1632,
    Rev32_32,
    Clz32,
    Cls32,
    Rbit64,
    Rev16_64,
    Rev32_64,
    Rev64,
    Clz64,
    Cls64,

    // Logical (shifted register)
    AndShiftedRegister32,
    BicShiftedRegister32,
    OrrShiftedRegister32,
    OrnShiftedRegister32,
    EorShiftedRegister32,
    EonShiftedRegister32,
    AndsShiftedRegister32,
    BicsShiftedRegister32,
    AndShiftedRegister64,
    BicShiftedRegister64,
    OrrShiftedRegister64,
    OrnShiftedRegister64,
    EorShiftedRegister64,
    EonShiftedRegister64,
    AndsShiftedRegister64,
    BicsShiftedRegister64,

    // Load/store register (unsigned immediate)
    StrbImm,
    LdrbImm,
    LdrsbImm32,
    LdrsbImm64,
    StrImmSimd8,
    LdrImmSimd8,
    StrImmSimd128,
    LdrImmSimd128,
    StrhImm,
    LdrhImm,
    LdrshImm64,
    LdrshImm32,
    StrImmSimd16,
    LdrImmSimd16,
    StrImm32,
    LdrImm32,
    LdrswImm,
    StrImmSimd32,
    LdrImmSimd32,
    StrImm64,
    LdrImm64,
    PrfmImm,
    StrImmSimd64,
    LdrImmSimd64,
}

impl OpCode {
    fn unconditional_branch_register(inst: u32) -> Self {
        // remove first 7 bits
        let masked = inst & (u32::MAX >> 7);

        let opc = masked >> 21;
        let op2 = (masked >> 16) & 0b11111;
        let op3 = (masked >> 10) & 0b111111;
        let rn = (masked >> 5) & 0b1111;
        let op4 = masked & 0b1111;

        match (opc, op2, op3, rn, op4) {
            (0b0000, 0b11111, 0b000000, _, op4) if op4 != 0 => todo!("unallocated"),
            (0b0000, 0b11111, 0b000000, _, 0b00000) => OpCode::Br,
            (0b0010, 0b11111, 0b000000, _, 0b00000) => OpCode::Ret,
            _ => todo!(),
        }
    }

    pub fn from_loads_and_stores(inst: u32) -> Self {
        let op0 = inst >> 28;
        let op1 = (inst >> 26) & 0b1;
        let op2 = (inst >> 23) & 0b11;
        let op3 = (inst >> 16) & 0b111111;
        let op4 = (inst >> 10) & 0b11;

        println!("{:b}", op1);

        match (op0, op1, op2, op3, op4) {
            (0b0000 | 0b0100, 0, 0b00, op3, _) if (op3 >> 5) == 1 => todo!("cmp+swap pair"),
            (0b0000 | 0b0100, 1, 0b00, 0b000000, _) => {
                todo!("Advanced SIMD load/store multiple structures")
            }
            (0b0000 | 0b0100, 0, 0b01, op3, _) if (op3 >> 5) == 0 => {
                todo!("Advanced SIMD load/store multiple structures (post-indexed) ")
            }
            (0b0001 | 0b0101 | 0b1001 | 0b1101, _, 0b00 | 0b01, _, _) => {
                todo!("Load register (literal)")
            }
            (0b0010 | 0b0110 | 0b1010 | 0b1110, _, 0b00, _, _) => {
                todo!("Load/store no-allocate pair (offset)")
            }
            (0b0010 | 0b0110 | 0b1010 | 0b1110, _, 0b01, _, _) => {
                todo!("Load/store register pair (post-indexed)")
            }
            (0b0010 | 0b0110 | 0b1010 | 0b1110, _, 0b10, _, _) => {
                todo!("Load/store register pair (offset)")
            }
            (0b0010 | 0b0110 | 0b1010 | 0b1110, _, 0b11, _, _) => {
                todo!("Load/store register pair (pre-indexed)")
            }
            (0b0011 | 0b0111 | 0b1011 | 0b1111, _, 0b00 | 0b01, op3, 0b10) if (op3 >> 5) == 1 => {
                todo!("Load/store register (register offset)")
            }
            (0b0011 | 0b0111 | 0b1011 | 0b1111, _, 0b00 | 0b01, op3, 0b01 | 0b11)
                if (op3 >> 5) == 1 =>
            {
                todo!("Load/store register (pac)")
            }
            (0b0011 | 0b0111 | 0b1011 | 0b1111, _, 0b10 | 0b11, _, _) => {
                let size = inst >> 30;
                let v = (inst >> 26) & 0b1;
                let opc = (inst >> 22) & 0b11;

                match (size, v, opc) {
                    (0b00, 0, 0b00) => OpCode::StrbImm,
                    (0b00, 0, 0b01) => OpCode::LdrbImm,
                    (0b00, 0, 0b10) => OpCode::LdrsbImm64,
                    (0b00, 0, 0b11) => OpCode::LdrsbImm32,
                    (0b00, 1, 0b00) => OpCode::StrImmSimd8,
                    (0b00, 1, 0b01) => OpCode::LdrImmSimd8,
                    (0b00, 1, 0b10) => OpCode::StrImmSimd128,
                    (0b00, 1, 0b11) => OpCode::LdrImmSimd128,
                    (0b01, 0, 0b00) => OpCode::StrhImm,
                    (0b01, 0, 0b01) => OpCode::LdrhImm,
                    (0b01, 0, 0b10) => OpCode::LdrshImm64,
                    (0b01, 0, 0b11) => OpCode::LdrshImm32,
                    (0b01, 1, 0b00) => OpCode::StrImmSimd16,
                    (0b01, 1, 0b01) => OpCode::LdrImmSimd16,
                    (0b10, 0, 0b00) => OpCode::StrImm32,
                    (0b10, 0, 0b01) => OpCode::LdrImm32,
                    (0b10, 0, 0b10) => OpCode::LdrswImm,
                    (0b10, 1, 0b00) => OpCode::StrImmSimd32,
                    (0b10, 1, 0b01) => OpCode::LdrImmSimd32,
                    (0b11, 0, 0b00) => OpCode::StrImm64,
                    (0b11, 0, 0b01) => OpCode::LdrImm64,
                    (0b11, 0, 0b10) => OpCode::PrfmImm,
                    (0b11, 1, 0b00) => OpCode::StrImmSimd64,
                    (0b11, 1, 0b01) => OpCode::LdrImmSimd64,
                    _ => todo!(),
                }
            }
            (..) => todo!(),
        }
    }

    pub fn from_branching(inst: u32) -> Self {
        let op0 = inst >> 29;

        // bits 12 to 25
        let op1 = (inst >> 12) & (u32::MAX >> 18);

        match op0 {
            0b010 => todo!("conditional branch immediate"),
            0b110 => match op1 {
                0b01000000110001 => todo!("system with register arg"),
                0b01000000110010 => todo!("hints"),
                0b01000000110011 => todo!("barriers"),
                _ if op1 >> 13 == 1 => Self::unconditional_branch_register(inst),
                _ => todo!(),
            },
            0b000 | 0b100 => todo!("unconditional branch immediate"),
            0b001 | 0b101 => todo!("cmp+branch imm and test+branch imm"),
            0b011 | 0b111 => todo!("unallocated"),
            _ => todo!(),
        }
    }

    pub fn from_data_processing_immediate(inst: u32) -> Self {
        let op0 = (inst & (u32::MAX >> 6)) >> 23;

        match op0 {
            0b000 | 0b001 => match inst >> 31 {
                0 => OpCode::Adr,
                1 => OpCode::Adrp,
                _ => unreachable!(),
            },
            0b010 => match inst >> 29 {
                0b000 => OpCode::AddImm32,
                0b001 => OpCode::AddsImm32,
                0b010 => OpCode::SubImm32,
                0b011 => OpCode::SubsImm32,
                0b100 => OpCode::AddImm64,
                0b101 => OpCode::AddsImm64,
                0b110 => OpCode::SubImm64,
                0b111 => OpCode::SubsImm64,
                _ => unreachable!(),
            },
            0b011 => match inst >> 29 {
                _ => todo!(),
            },
            0b100 => todo!(),
            0b101 => match inst >> 29 {
                0b000 => OpCode::Movn32,
                0b010 => OpCode::Movz32,
                0b011 => OpCode::Movk32,
                0b100 => OpCode::Movn64,
                0b110 => OpCode::Movz64,
                0b111 => OpCode::Movk64,
                _ => todo!(),
            },
            0b110 => todo!(),
            0b111 => todo!(),
            _ => unreachable!(),
        }
    }

    pub fn from_data_processing_register(inst: u32) -> Self {
        let op0 = (inst >> 30) & 0b1;
        let op1 = (inst >> 28) & 0b1;
        let op2 = (inst >> 21) & 0b1111;
        let op3 = (inst >> 10) & 0b111111;

        match (op0, op1, op2, op3) {
            (0, 1, 0b0110, _) => todo!("data processing (2 source)"),
            (1, 1, 0b0110, _) => todo!("data processing (1 source)"),
            (_, 0, op2, _) if (op2 >> 3) == 0 => {
                let sf = inst >> 31;
                let opc = (inst >> 29) & 0b11;
                let n = (inst >> 21) & 0b1;

                match (sf, opc, n) {
                    (0, 0b00, 0) => OpCode::AndShiftedRegister32,
                    (0, 0b00, 1) => OpCode::BicShiftedRegister32,
                    (0, 0b01, 0) => OpCode::OrrShiftedRegister32,
                    (0, 0b01, 1) => OpCode::OrnShiftedRegister32,
                    (0, 0b10, 0) => OpCode::EorShiftedRegister32,
                    (0, 0b10, 1) => OpCode::EonShiftedRegister32,
                    (0, 0b11, 0) => OpCode::AndsShiftedRegister32,
                    (0, 0b11, 1) => OpCode::BicsShiftedRegister32,
                    (1, 0b00, 0) => OpCode::AndShiftedRegister64,
                    (1, 0b00, 1) => OpCode::BicShiftedRegister64,
                    (1, 0b01, 0) => OpCode::OrrShiftedRegister64,
                    (1, 0b01, 1) => OpCode::OrnShiftedRegister64,
                    (1, 0b10, 0) => OpCode::EorShiftedRegister64,
                    (1, 0b10, 1) => OpCode::EonShiftedRegister64,
                    (1, 0b11, 0) => OpCode::AndsShiftedRegister64,
                    (1, 0b11, 1) => OpCode::BicsShiftedRegister64,
                    _ => todo!("unallocated"),
                }
            }
            (_, 0, 0b1000 | 0b1010 | 0b1100 | 0b1110, _) => todo!("add/sub (shifted register)"),
            (_, 0, 0b1001 | 0b1011 | 0b1101 | 0b1111, _) => todo!("add/sub (extended register)"),
            (_, 1, 0b0000, 0b000000) => todo!("add/sub (with carry)"),
            _ => todo!(),
        }
    }
}
