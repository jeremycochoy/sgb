use vm::*;
use tools::*;
use mmu;
use std::boxed::Box;

//////////////////////////////////////////////////////////
// Registers and utilitary functions to manipulate them
//////////////////////////////////////////////////////////

#[derive(PartialEq, Eq, Default, Debug)]
pub struct Registers {
        // Registers (a, b, c, d, e, h, l, f) :
        rs : [u8 ; 8],
        // Program counter
        pc : u16,
        // Stack pointer
        sp : u16,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
/// Name of the register
pub enum Register {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    E = 4,
    H = 5,
    L = 6,
    F = 7,
}

#[derive(PartialEq, Eq, Copy, Clone, Debug)]
/// List of flags
pub enum Flag {
    Z = 7,
    N = 6,
    H = 5,
    C = 4,
}

/// Macro for easy access to registers
///
/// Syntax : `reg![vm; register_name]`
/// where register_name : Register
macro_rules! reg {
    [$vm:expr ; $r:expr] => ($vm.cpu.registers.rs[$r as usize]);
}

/// Macro for accessing PC from a vm
///
/// Syntax : `pc![vm]`
macro_rules! pc {
    [$vm:expr] => ($vm.cpu.registers.pc);
}

/// Macro for acessing SP from a vm
///
/// Syntax : `sp![vm]`
macro_rules! sp {
    [$vm:expr] => ($vm.cpu.registers.pc);
}

/// Macro for acessing HL as a u16
/// (it's read only).
///
/// Syntax : `hl![vm]`
macro_rules! hl {
    [$vm:expr] => (
        w_combine(reg![$vm ; Register::H],
                  reg![$vm ; Register::L]
        )
    );
}

/// Macro for setting a u16 value into the register h:l
/// (the juxtaposition of the two registers)
macro_rules! set_hl {
    ($vm:expr, $value:expr) => {{
        let (h, l) = w_uncombine($value as u16);
        reg![$vm ; Register::H] = h;
        reg![$vm ; Register::L] = l;
    }}
}

/// Reset the flags of the Vm (set all flags to 0)
pub fn reset_flags(vm: &mut Vm) {
    reg![vm ; Register::F] = 0
}

/// Set the specified flag to the value given
pub fn set_flag(vm : &mut Vm, flag : Flag, value : bool) {
    if value {
        reg![vm ; Register::F] |= 1 << flag as usize
    }
    else {
        reg![vm ; Register::F] &= !(1 << flag as usize)
    }
}

/// Get the value from two registers h and l glued together (h:l)
pub fn get_r16(vm : &mut Vm, h : Register, l : Register) -> u16 {
    let initial_h = reg![vm ; h];
    let initial_l = reg![vm ; l];
    w_combine(initial_h, initial_l)
}

/// Set the value of two registers h and l glued together (h:l)
pub fn set_r16(vm : &mut Vm, h : Register, l : Register, value : u16) {
    let (value_h, value_l) = w_uncombine(value);
    reg![vm ; h] = value_h;
    reg![vm ; l] = value_l;
}

//////////////////////////////////////////
// CPU structurs, data types, and states
//////////////////////////////////////////

#[derive(PartialEq, Eq, Clone, Copy, Default, Debug)]
/// Represent a 'time' enlapsed
pub struct Clock {
    /// Length in byte of the last instruction
    m : u32,
    /// Duration in cycles
    t : u32,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum InterruptState {
        IEnabled,
        IDisabled,
}

impl Default for InterruptState {
    fn default() -> InterruptState { InterruptState::IDisabled }
}

#[derive(PartialEq, Eq, Default, Debug)]
pub struct Cpu {
        pub registers : Registers,
        pub clock : Clock,
        pub interrupt : InterruptState,
}

/// Read a byte from the memory pointed by PC, and increment PC
pub fn read_program_byte(vm : &mut Vm) -> u8 {
    pc![vm] = pc![vm].wrapping_add(1);
    mmu::rb(vm.cpu.registers.pc, &vm.mmu)
}

/// Read a word (2bytes) from the memory pointed by PC, and increment PC
pub fn read_program_word(vm : &mut Vm) -> u16 {
    pc![vm] = pc![vm].wrapping_add(2);
    mmu::rw(vm.cpu.registers.pc, &vm.mmu)
}

/// Store a CPU's instruction, that is a string describing the assembly instruction, and the *function pointer*
pub struct Instruction(&'static str, Box<Fn(&mut Vm) -> Clock>);

/// Execute exactly one instruction by the CPU
///
/// The function load the byte pointed by PC, increment PC,
/// and call dispatch with the opcode to run the instruction.
pub fn execute_one_instruction(vm : &mut Vm) {
    // Disable bios if needed
    if pc![vm] >= 0x100 {
        vm.mmu.bios_enabled = false;
    }

    // Run the instruction
    let opcode = read_program_byte(vm);
    let Instruction(name, fct) = match opcode {
        0xCB => dispatch_cb(read_program_byte(vm)),
        _    => dispatch(opcode),
    };

    let clock = (fct)(vm);
    println!("{:?} {}", clock, name);

    // Update GPU's mode (Clock, Scanline, VBlank, HBlank, ...)
    //update_gpu_mode(&mut vm.gpu, clock.t);
}

/// Simple macro for writing dispatch more easily
macro_rules! mk_inst {
    [$vm:ident > $name:expr , $f:expr] => {{
        Instruction($name, Box::new(|$vm : &mut Vm| $f))
    }}
}

/// Associate to each opcode:u8 it's instruction:Instruction
pub fn dispatch(opcode : u8) -> Instruction {
    match opcode {
        0x00 => mk_inst![vm> "NOP",     i_nop(vm)],
        0x01 => mk_inst![vm> "LDBCd16", i_ldr16d16(vm, Register::B, Register::C)],
        0x02 => mk_inst![vm> "LDBCmA",  i_ldr16mr(vm, Register::B, Register::C, Register::A)],
        0x03 => mk_inst![vm> "INCBC",   i_incr16(vm, Register::B, Register::C)],
        0x04 => mk_inst![vm> "INCB",    i_incr(vm, Register::B)],
        0x05 => mk_inst![vm> "DECB",    i_decr(vm, Register::B)],
        0x06 => mk_inst![vm> "LDBd8",   i_ldrd8(vm, Register::B)],
        //0x07 =>
        //0x08 =>
        //0x09 =>
        0x0A => mk_inst![vm> "LDABCm",  i_ldrr16m(vm, Register::A, Register::B, Register::C)],
        0x0B => mk_inst![vm> "DECBC",   i_decr16(vm, Register::B, Register::C)],
        0x0C => mk_inst![vm> "INCC",    i_incr(vm, Register::C)],
        0x0D => mk_inst![vm> "DECC",    i_decr(vm, Register::C)],
        0x0E => mk_inst![vm> "LDCd8",   i_ldrd8(vm, Register::C)],
        //0x0F =>

        0x11 => mk_inst![vm> "LDDEd16", i_ldr16d16(vm, Register::D, Register::E)],
        0x12 => mk_inst![vm> "LDDEmA",  i_ldr16mr(vm, Register::D, Register::E, Register::A)],
        0x13 => mk_inst![vm> "INCDE",   i_incr16(vm, Register::D, Register::E)],
        0x14 => mk_inst![vm> "INCD",    i_incr(vm, Register::D)],
        0x15 => mk_inst![vm> "DECD",    i_decr(vm, Register::D)],
        0x16 => mk_inst![vm> "LDDd8",   i_ldrd8(vm, Register::D)],
        0x1A => mk_inst![vm> "LDADEm",  i_ldrr16m(vm, Register::A, Register::D, Register::E)],
        0x1B => mk_inst![vm> "DECDE",   i_decr16(vm, Register::D, Register::E)],
        0x1C => mk_inst![vm> "INCE",    i_incr(vm, Register::E)],
        0x1D => mk_inst![vm> "DECE",    i_decr(vm, Register::E)],
        0x1E => mk_inst![vm> "LDEd8",   i_ldrd8(vm, Register::E)],
        // 0x1F =>

        0x21 => mk_inst![vm> "LDHLd16", i_ldr16d16(vm, Register::H, Register::L)],
        0x22 => mk_inst![vm> "LDIHLmA", i_ldihlma(vm)],
        0x23 => mk_inst![vm> "INCHL",   i_incr16(vm, Register::H, Register::L)],
        0x24 => mk_inst![vm> "INCH",    i_incr(vm, Register::H)],
        0x25 => mk_inst![vm> "DECH",    i_decr(vm, Register::H)],
        0x26 => mk_inst![vm> "LDHd8",   i_ldrd8(vm, Register::H)],
        // 0x27 =>
        // 0x28 =>
        // 0x29 =>
        0x2A => mk_inst![vm> "LDIAHLm", i_ldiahlm(vm)],
        0x2B => mk_inst![vm> "DECHL",   i_decr16(vm, Register::H, Register::L)],
        0x2C => mk_inst![vm> "INCL",    i_incr(vm, Register::L)],
        0x2D => mk_inst![vm> "DECL",    i_decr(vm, Register::L)],
        0x2E => mk_inst![vm> "LDLd8",   i_ldrd8(vm, Register::L)],
        // 0x2F =>

        0x31 => mk_inst![vm> "LDSPd16", i_ldspd16(vm)],
        0x32 => mk_inst![vm> "LDDHLmA", i_lddhlma(vm)],
        0x33 => mk_inst![vm> "INSP",    i_incsp(vm)],
        0x34 => mk_inst![vm> "INHLm",   i_inchlm(vm)],
        0x35 => mk_inst![vm> "DECHLm",  i_dechlm(vm)],
        0x36 => mk_inst![vm> "LDHLmd8", i_ldhlmd8(vm)],
        // 0x37 =>
        // 0x38 =>
        // 0x39 =>
        0x3A => mk_inst![vm> "LDDAHLm", i_lddahlm(vm)],
        0x3B => mk_inst![vm> "DECSP",   i_decsp(vm)],
        0x3C => mk_inst![vm> "INCA",    i_incr(vm, Register::A)],
        0x3D => mk_inst![vm> "DECA",    i_decr(vm, Register::A)],
        0x3E => mk_inst![vm> "LDAd8",   i_ldrd8(vm, Register::A)],
        // 0x3F =>

        0x40 => mk_inst![vm> "LDBB",    i_ldrr(vm, Register::B, Register::B)],
        0x41 => mk_inst![vm> "LDBC",    i_ldrr(vm, Register::B, Register::C)],
        0x42 => mk_inst![vm> "LDBD",    i_ldrr(vm, Register::B, Register::D)],
        0x43 => mk_inst![vm> "LDBE",    i_ldrr(vm, Register::B, Register::E)],
        0x44 => mk_inst![vm> "LDBH",    i_ldrr(vm, Register::B, Register::H)],
        0x45 => mk_inst![vm> "LDBL",    i_ldrr(vm, Register::B, Register::L)],
        0x46 => mk_inst![vm> "LDBHLm",  i_ldrr16m(vm, Register::B, Register::H, Register::L)],
        0x47 => mk_inst![vm> "LDBA",    i_ldrr(vm, Register::B, Register::A)],
        0x48 => mk_inst![vm> "LDCB",    i_ldrr(vm, Register::C, Register::B)],
        0x49 => mk_inst![vm> "LDCC",    i_ldrr(vm, Register::C, Register::C)],
        0x4A => mk_inst![vm> "LDCD",    i_ldrr(vm, Register::C, Register::D)],
        0x4B => mk_inst![vm> "LDCE",    i_ldrr(vm, Register::C, Register::E)],
        0x4C => mk_inst![vm> "LDCH",    i_ldrr(vm, Register::C, Register::H)],
        0x4D => mk_inst![vm> "LDCL",    i_ldrr(vm, Register::C, Register::L)],
        0x4E => mk_inst![vm> "LDCHLm",  i_ldrr16m(vm, Register::C, Register::H, Register::L)],
        0x4F => mk_inst![vm> "LDCA",    i_ldrr(vm, Register::C, Register::A)],

        0x50 => mk_inst![vm> "LDDB",    i_ldrr(vm, Register::D, Register::B)],
        0x51 => mk_inst![vm> "LDDC",    i_ldrr(vm, Register::D, Register::C)],
        0x52 => mk_inst![vm> "LDDD",    i_ldrr(vm, Register::D, Register::D)],
        0x53 => mk_inst![vm> "LDDE",    i_ldrr(vm, Register::D, Register::E)],
        0x54 => mk_inst![vm> "LDDH",    i_ldrr(vm, Register::D, Register::H)],
        0x55 => mk_inst![vm> "LDDL",    i_ldrr(vm, Register::D, Register::L)],
        0x56 => mk_inst![vm> "LDDHLm",  i_ldrr16m(vm, Register::D, Register::H, Register::L)],
        0x57 => mk_inst![vm> "LDDA",    i_ldrr(vm, Register::D, Register::A)],
        0x58 => mk_inst![vm> "LDEB",    i_ldrr(vm, Register::E, Register::B)],
        0x59 => mk_inst![vm> "LDEC",    i_ldrr(vm, Register::E, Register::C)],
        0x5A => mk_inst![vm> "LDED",    i_ldrr(vm, Register::E, Register::D)],
        0x5B => mk_inst![vm> "LDEE",    i_ldrr(vm, Register::E, Register::E)],
        0x5C => mk_inst![vm> "LDEH",    i_ldrr(vm, Register::E, Register::H)],
        0x5D => mk_inst![vm> "LDEL",    i_ldrr(vm, Register::E, Register::L)],
        0x5E => mk_inst![vm> "LDEHLm",  i_ldrr16m(vm, Register::E, Register::H, Register::L)],
        0x5F => mk_inst![vm> "LDEA",    i_ldrr(vm, Register::E, Register::A)],

        0x60 => mk_inst![vm> "LDHB",    i_ldrr(vm, Register::H, Register::B)],
        0x61 => mk_inst![vm> "LDHC",    i_ldrr(vm, Register::H, Register::C)],
        0x62 => mk_inst![vm> "LDHD",    i_ldrr(vm, Register::H, Register::D)],
        0x63 => mk_inst![vm> "LDHE",    i_ldrr(vm, Register::H, Register::E)],
        0x64 => mk_inst![vm> "LDHH",    i_ldrr(vm, Register::H, Register::H)],
        0x65 => mk_inst![vm> "LDHL",    i_ldrr(vm, Register::H, Register::L)],
        0x66 => mk_inst![vm> "LDHHLm",  i_ldrr16m(vm, Register::H, Register::H, Register::L)],
        0x67 => mk_inst![vm> "LDHA",    i_ldrr(vm, Register::H, Register::A)],
        0x68 => mk_inst![vm> "LDLB",    i_ldrr(vm, Register::L, Register::B)],
        0x69 => mk_inst![vm> "LDLC",    i_ldrr(vm, Register::L, Register::C)],
        0x6A => mk_inst![vm> "LDLD",    i_ldrr(vm, Register::L, Register::D)],
        0x6B => mk_inst![vm> "LDLE",    i_ldrr(vm, Register::L, Register::E)],
        0x6C => mk_inst![vm> "LDLH",    i_ldrr(vm, Register::L, Register::H)],
        0x6D => mk_inst![vm> "LDLL",    i_ldrr(vm, Register::L, Register::L)],
        0x6E => mk_inst![vm> "LDLHLm",  i_ldrr16m(vm, Register::L, Register::H, Register::L)],
        0x6F => mk_inst![vm> "LDLA",    i_ldrr(vm, Register::L, Register::A)],

        0x70 => mk_inst![vm> "LDHLmB",  i_ldr16mr(vm, Register::H, Register::L, Register::B)],
        0x71 => mk_inst![vm> "LDHLmC",  i_ldr16mr(vm, Register::H, Register::L, Register::C)],
        0x72 => mk_inst![vm> "LDHLmD",  i_ldr16mr(vm, Register::H, Register::L, Register::D)],
        0x73 => mk_inst![vm> "LDHLmE",  i_ldr16mr(vm, Register::H, Register::L, Register::E)],
        0x74 => mk_inst![vm> "LDHLmH",  i_ldr16mr(vm, Register::H, Register::L, Register::H)],
        0x75 => mk_inst![vm> "LDHLmL",  i_ldr16mr(vm, Register::H, Register::L, Register::L)],
        // 0x76 => HALT
        0x77 => mk_inst![vm> "LDHLmA",  i_ldr16mr(vm, Register::H, Register::L, Register::A)],
        0x78 => mk_inst![vm> "LDAB",    i_ldrr(vm, Register::A, Register::B)],
        0x79 => mk_inst![vm> "LDAC",    i_ldrr(vm, Register::A, Register::C)],
        0x7A => mk_inst![vm> "LDAD",    i_ldrr(vm, Register::A, Register::D)],
        0x7B => mk_inst![vm> "LDAE",    i_ldrr(vm, Register::A, Register::E)],
        0x7C => mk_inst![vm> "LDAH",    i_ldrr(vm, Register::A, Register::H)],
        0x7D => mk_inst![vm> "LDAL",    i_ldrr(vm, Register::A, Register::L)],
        0x7E => mk_inst![vm> "LDAHLm",  i_ldrr16m(vm, Register::A, Register::H, Register::L)],
        0x7F => mk_inst![vm> "LDAA",    i_ldrr(vm, Register::A, Register::A)],

        0xA8 => mk_inst![vm> "XORb",    i_xorr(vm, Register::B)],
        0xA9 => mk_inst![vm> "XORc",    i_xorr(vm, Register::C)],
        0xAA => mk_inst![vm> "XORd",    i_xorr(vm, Register::D)],
        0xAB => mk_inst![vm> "XORe",    i_xorr(vm, Register::E)],
        0xAC => mk_inst![vm> "XORh",    i_xorr(vm, Register::H)],
        0xAD => mk_inst![vm> "XORl",    i_xorr(vm, Register::L)],
        0xAE => mk_inst![vm> "XORhlm",  i_xorhlm(vm)],
        0xAF => mk_inst![vm> "XORa",    i_xorr(vm, Register::A)],

        0xB0 => mk_inst![vm> "ORB",     i_orr(vm, Register::B)],
        0xB1 => mk_inst![vm> "ORC",     i_orr(vm, Register::C)],
        0xB2 => mk_inst![vm> "ORD",     i_orr(vm, Register::D)],
        0xB3 => mk_inst![vm> "ORE",     i_orr(vm, Register::E)],
        0xB4 => mk_inst![vm> "ORH",     i_orr(vm, Register::H)],
        0xB5 => mk_inst![vm> "ORL",     i_orr(vm, Register::L)],
        0xB6 => mk_inst![vm> "ORHLm",   i_orhlm(vm)],
        0xB7 => mk_inst![vm> "ORA",     i_orr(vm, Register::A)],
        0xB8 => mk_inst![vm> "CPB",     i_cpr(vm, Register::B)],
        0xB9 => mk_inst![vm> "CPC",     i_cpr(vm, Register::C)],
        0xBA => mk_inst![vm> "CPD",     i_cpr(vm, Register::D)],
        0xBB => mk_inst![vm> "CPE",     i_cpr(vm, Register::E)],
        0xBC => mk_inst![vm> "CPH",     i_cpr(vm, Register::H)],
        0xBD => mk_inst![vm> "CPL",     i_cpr(vm, Register::L)],
        0xBE => mk_inst![vm> "CPHLm",   i_cphlm(vm)],
        0xBF => mk_inst![vm> "CPA",     i_cpr(vm, Register::A)],

        0xCB => Instruction("CBPref", Box::new(|_ : &mut Vm| Clock { m:0, t:0 })),

        0xEE => mk_inst![vm> "XORd8",   i_xord8(vm)],

        0xF6 => mk_inst![vm> "ORd8",    i_ord8(vm)],
        0xFE => mk_inst![vm> "CPd8",    i_cpd8(vm)],

        _ => panic!(format!("missing instruction 0x{:2X} !", opcode)),
    }
}

/// Associate to each opcode:u8 it's instruction:Instruction in the 0xCB table
pub fn dispatch_cb(opcode : u8) -> Instruction {
    match opcode {
        // 0x00 =>
        _ => panic!(format!("CB Prefix : missing instruction 0xCB:0x{:2X} !", opcode)),
    }
}

/////////////////////////////////////////
//
// Implementation of the CPU instructions
//
/////////////////////////////////////////

/// No Operation
pub fn i_nop(_ : &mut Vm) -> Clock {
    Clock { m:1, t:4 }
}

/// LD (Load) instruction
///
/// Syntax : `LD vm:Vm dst:Register src:Register`
///
/// > LD Register <- Register
pub fn i_ldrr(vm : &mut Vm, dst : Register, src : Register) -> Clock {
    reg![vm; dst] = reg![vm; src];
    Clock { m:1, t:4 }
}

/// Same as LD, but alow to use (h:l) on the right side
///
/// Syntax : `LDrr16m vm:Vm h:Register l:Register`
///
/// > LDrr16m Register <- (h:l)
pub fn i_ldrr16m(vm : &mut Vm, dst : Register, h : Register, l : Register) -> Clock {
    let addr = get_r16(vm, h, l);
    reg![vm ; dst] = mmu::rb(addr, &vm.mmu);
    Clock { m:1, t:8 }
}

/// Same as LD, but alow to use (h:l) on the left side
///
/// Syntax : `LDhlr vm:Vm h:Register l:Register`
///
/// > LDr16mr (h:l) <- Register
pub fn i_ldr16mr(vm : &mut Vm, h : Register, l : Register, src : Register) -> Clock {
    let addr = get_r16(vm, h, l);
    mmu::wb(addr, reg![vm ; src], &mut vm.mmu);
    Clock { m:1, t:8 }
}

/// Implementation for LD[I|D] (HL) A
pub fn i_ldmod_hla(vm : &mut Vm, modificator : i16) -> Clock {
    mmu::wb(hl![vm], reg![vm ; Register::A], &mut vm.mmu);

    let sum = (hl![vm] as i16).wrapping_add(modificator);
    set_hl!(vm, sum);
    Clock { m:1, t:8 }
}

/// Implementation for LD[I|D] A (HL)
pub fn i_ldmod_ahl(vm : &mut Vm, modificator : i16) -> Clock {
    reg![vm ; Register::A] = mmu::rb(hl![vm], &mut vm.mmu);

    let sum = (hl![vm] as i16).wrapping_add(modificator);
    set_hl!(vm, sum);
    Clock { m:1, t:8 }
}

/// Load the value of A in (HL) and increment HL
///
/// > LDI (HL+) <- A
pub fn i_ldihlma(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, 1)}
/// Load the value of (HL) in A and increment HL
///
/// > LDI A <- (HL+)
pub fn i_ldiahlm(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, 1)}
/// Load the value of A in (HL) and decrement HL
///
/// > LDD (HL-) <- A
pub fn i_lddhlma(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, -1)}
/// Load the value of (HL) in A and decrement HL
///
/// > LDD A <- (HL-)
pub fn i_lddahlm(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, -1)}

/// LD Register <- immediate Word8
pub fn i_ldrd8(vm : &mut Vm, dst : Register) -> Clock {
    reg![vm ; dst] = read_program_byte(vm);
    Clock { m:2, t:8 }
}

/// LD (HL) <- immediate Word8
pub fn i_ldhlmd8(vm : &mut Vm) -> Clock {
    mmu::wb(hl![vm], read_program_byte(vm), &mut vm.mmu);
    Clock { m:2, t:8 }
}

/// LD (a16) <- a where a16 means the next Word16 as an address
pub fn i_lda16a(vm : &mut Vm) -> Clock {
    let a16 = read_program_word(vm);
    mmu::wb(a16, reg![vm ; Register::A], &mut vm.mmu);
    Clock { m:3, t:12 }
}

/// LD a <- (a16) where a16 means the next Word16 as an address
pub fn i_ldaa16(vm : &mut Vm) -> Clock {
    let a16 = read_program_word(vm);
    reg![vm ; Register::A] = mmu::rb(a16, &vm.mmu);
    Clock { m:3, t:12 }
}

/// LD r16 <- d16 where d16 means direct Word8 value
pub fn i_ldr16d16(vm : &mut Vm, h : Register, l : Register) -> Clock {
    let d16 = read_program_word(vm);
    set_r16(vm, h, l, d16);
    Clock { m:3, t:12 }
}

/// LD (a8) <- a where a8 means the next Word8 + 0xFF00 as an address
pub fn i_lda8a(vm : &mut Vm) -> Clock {
    let a8 = read_program_byte(vm);
    mmu::wb(a8 as u16 + 0xFF00, reg![vm ; Register::A], &mut vm.mmu);
    Clock { m:3, t:12 }
}

/// LD a <- (a8) where a8 means the next Word8 + 0xFF00 as an address
pub fn i_ldaa8(vm : &mut Vm) -> Clock {
    let a8 = read_program_byte(vm);
    reg![vm ; Register::A] = mmu::rb(a8 as u16 + 0xFF00, &vm.mmu);
    Clock { m:3, t:12 }
}

/// LD SP <- d16 where d16 means direct Word8 value
pub fn i_ldspd16(vm : &mut Vm) -> Clock {
    let d16 = read_program_word(vm);
    sp![vm] = d16;
    Clock { m:3, t:12 }
}

/// Implement xoring the register A with the value src_val
pub fn i_xor_imp(src_val : u8, vm : &mut Vm) {
    reg![vm ; Register::A] ^= src_val;
    let result = reg![vm ; Register::A];
    set_flag(vm, Flag::Z, result == 0);
}

/// XOR the register A with a register src into A
/// Syntax : `XOR src:Register`
pub fn i_xorr(vm : &mut Vm, src : Register) -> Clock {
    reset_flags(vm);
    i_xor_imp(reg![vm ; src], vm);
    Clock { m:1, t:8 }
}

/// XOR the register A with (HL) into A
/// Syntax : `XORHLm`
pub fn i_xorhlm(vm : &mut Vm) -> Clock {
    reset_flags(vm);
    i_xor_imp(mmu::rb(hl![vm], &vm.mmu), vm);
    Clock { m:1, t:8 }
}

/// XOR the register A with immediate word8 into A
/// Syntax : `XORd8`
pub fn i_xord8(vm : &mut Vm) -> Clock {
    reset_flags(vm);
    let d8 = read_program_byte(vm);
    i_xor_imp(d8, vm);
    Clock { m:1, t:8 }
}

/// Implementation OR of a value with the register A, stored into A
pub fn i_or_imp(src_val : u8, vm : &mut Vm) {
    reset_flags(vm);
    reg![vm ; Register::A] |= src_val;
    let result = reg![vm ; Register::A];
    set_flag(vm, Flag::Z, result == 0);
}

/// Bitwise OR the register A with a register src into A
/// Syntax : `OR src`
pub fn i_orr(vm : &mut Vm, src : Register) -> Clock {
    i_or_imp(reg![vm ; src], vm);
    Clock { m:1, t:4 }
}

/// Bitwise OR the register A with (HL) into A
/// Syntax : `ORHLm`
pub fn i_orhlm(vm : &mut Vm) -> Clock {
    i_or_imp(mmu::rb(hl![vm], &vm.mmu), vm);
    Clock { m:1, t:8 }
}

/// Bitwise OR the register A with the immediate word8 into A
/// Syntax : `ORd8`
pub fn i_ord8(vm : &mut Vm) -> Clock {
    i_or_imp(mmu::rb(hl![vm], &vm.mmu), vm);
    Clock { m:1, t:8 }
}

/// Implementation of the increment instruction (setting flags)
pub fn i_inc_impl(vm : &mut Vm, initial_val : u8, final_val : u8) {
    reset_flags(vm);
    set_flag(vm, Flag::Z, final_val == 0);
    set_flag(vm, Flag::H, (initial_val & 0x0F + 1 > 0x0F));
    set_flag(vm, Flag::N, false);
}

/// Increment the register given, and set Z, H as expected.
/// Always set N to 0.
///
/// Syntax : `INC reg:Register`
pub fn i_incr(vm : &mut Vm, reg : Register) -> Clock {
    let initial_val = reg![vm ; reg];
    reg![vm ; reg] = reg![vm ; reg].wrapping_add(1);
    let final_val = reg![vm ; reg];
    i_inc_impl(vm, initial_val, final_val);
    Clock { m:1, t:4 }
}

/// Increment (HL), and set Z, H as expected.
/// Always set N to 0.
///
/// Syntax : `INCHLm`
pub fn i_inchlm(vm : &mut Vm) -> Clock {
    let initial_val = mmu::rb(hl![vm], &vm.mmu);
    let final_val = initial_val.wrapping_add(1);
    mmu::wb(hl![vm], final_val, &mut vm.mmu);
    i_inc_impl(vm, initial_val, final_val);
    Clock { m:1, t:12 }
}

/// Increment the 16 bits register given.
/// Leave flags unaffected.
///
/// Syntax : `INC hight:Register low:Register`
pub fn i_incr16(vm : &mut Vm, h : Register, l : Register) -> Clock {
    let initial_val = get_r16(vm, h, l);
    let final_val = initial_val.wrapping_add(1);
    set_r16(vm, h, l, final_val);

    Clock { m:1, t:8 }
}

/// Increment the register SP
/// Leave flags unaffected.
///
/// Syntax : `INCSP`
pub fn i_incsp(vm : &mut Vm) -> Clock {
    sp![vm] = sp![vm].wrapping_add(1);

    Clock { m:1, t:8 }
}

/// Implementation of the increment instruction (setting flags)
pub fn i_dec_impl(vm : &mut Vm, initial_val : u8, final_val : u8) {
    reset_flags(vm);
    set_flag(vm, Flag::Z, final_val == 0);
    // intial_val - 1 == initial_val + 0xFF
    set_flag(vm, Flag::H, (initial_val & 0x0F + 0x0F > 0x0F));
    set_flag(vm, Flag::N, false);
}

/// Decrement the register given, and set Z, H as expected.
/// Always set N to 0.
///
/// Syntax : `DEC reg:Register`
pub fn i_decr(vm : &mut Vm, reg : Register) -> Clock {
    let initial_val = reg![vm ; reg];
    let final_val = initial_val.wrapping_sub(1);
    reg![vm ; reg] = final_val;
    i_dec_impl(vm, initial_val, final_val);
    Clock { m:1, t:4 }
}

/// Decrement (HL), and set Z, H as expected.
/// Always set N to 0.
///
/// Syntax : `INCHLm`
pub fn i_dechlm(vm : &mut Vm) -> Clock {
    let initial_val = mmu::rb(hl![vm], &vm.mmu);
    let final_val = initial_val.wrapping_sub(1);
    mmu::wb(hl![vm], final_val, &mut vm.mmu);
    i_dec_impl(vm, initial_val, final_val);
    Clock { m:1, t:12 }
}

/// Decrement the 16 bits register given.
/// Leave flags unaffected.
///
/// Syntax : `DEC hight:Register low:Register`
pub fn i_decr16(vm : &mut Vm, h : Register, l : Register) -> Clock {
    let initial_val = get_r16(vm, h, l);
    let final_val = initial_val.wrapping_sub(1);
    set_r16(vm, h, l, final_val);

    Clock { m:1, t:8 }
}

/// Decrement the register SP
/// Leave flags unaffected.
///
/// Syntax : `DECSP`
pub fn i_decsp(vm : &mut Vm) -> Clock {
    sp![vm] = sp![vm].wrapping_sub(1);

    Clock { m:1, t:8 }
}

/// Compare src:Register with A and set the flags Z/H/C.
/// Set register N to 1.
///
/// Syntax : `CP src:Register`
pub fn i_cpr(vm : &mut Vm, src : Register) -> Clock {
    let input = reg![vm ; src];

    // Update flags and discard result
    i_sub_imp(vm, input);

    Clock { m:1, t:4 }
}

/// Compare (HL) with A and set the flags Z/H/C.
/// Set register N to 1.
///
/// Syntax : `CPHLm`
pub fn i_cphlm(vm : &mut Vm) -> Clock {
    let input = mmu::rb(hl![vm], &mut vm.mmu);

    // Update flags and discard result
    i_sub_imp(vm, input);

    Clock { m:1, t:8 }
}

/// Compare direct Word8 with A and set the flags Z/H/C.
/// Set register N to 1.
///
/// Syntax : `CPd8`
pub fn i_cpd8(vm : &mut Vm) -> Clock {
    let input = read_program_byte(vm);

    // Update flags and discard result
    i_sub_imp(vm, input);

    Clock { m:2, t:8 }
}

/// Implement substracting value:u8 to the register A and set the flags
pub fn i_sub_imp(vm : &mut Vm, value : u8) -> u8 {
    let a = reg![vm ; Register::A];
    let b = value;
    let diff = a.wrapping_sub(b);
    reset_flags(vm);
    set_flag(vm, Flag::Z, diff == 0);
    set_flag(vm, Flag::N, true);
    set_flag(vm, Flag::H, (0x0F & a).wrapping_sub(0x0F & b) > 0xF);
    set_flag(vm, Flag::C, b > a);
    return diff
}
