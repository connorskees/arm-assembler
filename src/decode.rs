use crate::{Instruction, InstructionKind, OpCode, Operands, Shift};

pub(crate) struct InstructionDecoder {}

impl InstructionDecoder {
    pub fn decode(inst: u32) -> Instruction {
        let kind = InstructionKind::from_integer(inst);

        println!("{:0>32b}", inst);

        match kind {
            InstructionKind::DataProcessingImmediate => Self::parse_data_processing_immediate(inst),
            InstructionKind::DataProcessingRegister => Self::parse_data_processing_register(inst),
            InstructionKind::Branching => Self::parse_branching(inst),
            InstructionKind::LoadsAndStores => Self::parse_loads_and_stores(inst),
            _ => todo!("{:?}", kind),
        }
    }

    fn parse_loads_and_stores(inst: u32) -> Instruction {
        let op_code = OpCode::from_loads_and_stores(inst);

        let operands = Operands::zero();

        Instruction {
            kind: InstructionKind::LoadsAndStores,
            op_code,
            operands,
            shift: Shift::Lsl,
        }
    }

    fn parse_data_processing_immediate(inst: u32) -> Instruction {
        let op_code = OpCode::from_data_processing_immediate(inst);

        let operands = Operands::zero();

        Instruction {
            kind: InstructionKind::DataProcessingImmediate,
            op_code,
            operands,
            shift: Shift::Lsl,
        }
    }

    fn parse_data_processing_register(inst: u32) -> Instruction {
        let op_code = OpCode::from_data_processing_register(inst);

        let operands = Operands::zero();

        Instruction {
            kind: InstructionKind::DataProcessingRegister,
            op_code,
            operands,
            shift: Shift::Lsl,
        }
    }

    fn parse_branching(inst: u32) -> Instruction {
        let op_code = OpCode::from_branching(inst);

        let operands = Operands::zero();

        Instruction {
            kind: InstructionKind::Branching,
            op_code,
            operands,
            shift: Shift::Lsl,
        }
    }
}
