use crate::{Instruction, InstructionKind, OpCode, Operands, Register, Shift};

pub(crate) struct InstructionEncoder {
    operands: Operands,
    op_code: OpCode,
    shift: Shift,
}

impl InstructionEncoder {
    pub fn encode(inst: Instruction, buffer: &mut Vec<u8>) {
        let encoder = InstructionEncoder {
            operands: inst.operands,
            op_code: inst.op_code,
            shift: inst.shift,
        };

        match inst.kind {
            InstructionKind::DataProcessingImmediate => {
                encoder.encode_data_processing_immediate(buffer)
            }
            InstructionKind::Branching => encoder.encode_branching(buffer),
            InstructionKind::DataProcessingRegister => {
                encoder.encode_data_processing_register(buffer)
            }
            _ => todo!("{:?}", inst.kind),
        }
    }

    fn encode_data_processing_register(&self, buffer: &mut Vec<u8>) {
        let mut inst: u32 = 0b0101010 << 24;

        match self.op_code {
            OpCode::OrrShiftedRegister32 | OpCode::OrrShiftedRegister64 => {
                inst |= (self.shift as u32) << 21;

                let rm = self.operands.get(0).expect_register();
                let imm6 = self.operands.get(1).expect_imm6() as u32;
                let rn = self.operands.get(2).expect_register();
                let rd = self.operands.get(3).expect_register();

                inst |= rm.encoding() << 16;
                inst |= imm6 << 10;
                inst |= rn.encoding() << 5;
                inst |= rd.encoding();
            }
            _ => todo!(),
        }

        if self.op_code == OpCode::OrrShiftedRegister64 {
            inst |= 1 << 31;
        }

        println!("{:0>32b} -- inst", inst);

        buffer.extend_from_slice(&inst.to_le_bytes());
    }

    fn encode_branching(&self, buffer: &mut Vec<u8>) {
        match self.op_code {
            OpCode::Br | OpCode::Ret => self.encode_unconditional_branch_register(buffer),
            _ => todo!(),
        }
    }

    fn encode_unconditional_branch_register(&self, buffer: &mut Vec<u8>) {
        let mut inst: u32 = 0;

        inst |= 0b1101011 << (32 - 7);

        match self.op_code {
            OpCode::Ret => {
                let register = self.operands.get(0).get_register().unwrap_or(Register::X30);

                inst |= 0b0010_11111 << 16;
                inst |= register.encoding() << 5;
            }
            _ => todo!(),
        }

        println!("{:0>32b}", inst);

        // println!("{:b}", inst);

        buffer.extend_from_slice(&inst.to_le_bytes());
    }

    fn encode_data_processing_immediate(&self, buffer: &mut Vec<u8>) {
        let mut inst = 0;

        match self.op_code {
            OpCode::AddImm32 => todo!(),
            OpCode::Movn32 => todo!(),
            OpCode::Movz32 => {
                inst |= 0b0101001010 << (32 - 10);

                let imm = self.operands.get(0).expect_imm16();
                let register = self.operands.get(1).expect_register();

                inst |= (imm << 5) as u32;
                inst |= register.encoding();
            }
            _ => todo!(),
        }
        // let mut inst = 0;

        // inst |= (self.is_32_bit as u8) << 31;

        println!("{:0>32b}", inst);

        buffer.extend_from_slice(&inst.to_le_bytes());
    }
}
