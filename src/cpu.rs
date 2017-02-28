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
    pc![vm] += 1;
    mmu::rb(vm.cpu.registers.pc, &vm.mmu)
}

/// Read a word (2bytes) from the memory pointed by PC, and increment PC
pub fn read_program_word(vm : &mut Vm) -> u16 {
    pc![vm] += 2;
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
    let clock = dispatch(opcode);

    // Update GPU's mode (Clock, Scanline, VBlank, HBlank, ...)
    //update_gpu_mode(&mut vm.gpu, clock.t);
}

/// Associate to each opcode:u8 it's instruction:Instruction
pub fn dispatch(opcode : u8) -> Instruction {
    match opcode {
        0x00 => Instruction("NOP", Box::new(i_nop)),
        0x01 => Instruction("LDBCd16", Box::new(|vm : &mut Vm| i_ldr16d16(vm, Register::B, Register::C))),
        0x02 => Instruction("LD(BC)A", Box::new(|vm : &mut Vm| i_ldr16mr(vm, Register::B, Register::C, Register::A))),
        0x03 => Instruction("INCBC", Box::new(|vm : &mut Vm| i_incr16(vm, Register::B, Register::C))),
        0x04 => Instruction("INCB", Box::new(|vm : &mut Vm| i_incr(vm, Register::B))),
        0x05 => Instruction("DECb", Box::new(|vm : &mut Vm| i_decr(vm, Register::B))),
        0x06 => Instruction("LDBd8", Box::new(|vm : &mut Vm| i_ldrd8(vm, Register::B))),
        //0x07 =>
        //0x08 =>
        //0x09 =>
        0x0A => Instruction("LDABCm", Box::new(|vm : &mut Vm| i_ldrr16m(vm, Register::A, Register::B, Register::C))),
        0x0B => Instruction("DECBC", Box::new(|vm : &mut Vm| i_decr16(vm, Register::B, Register::C))),
        0x0C => Instruction("INCC", Box::new(|vm : &mut Vm| i_incr(vm, Register::C))),
        0x0D => Instruction("DECC", Box::new(|vm : &mut Vm| i_decr(vm, Register::C))),
        0x0E => Instruction("LDCd8", Box::new(|vm : &mut Vm| i_ldrd8(vm, Register::C))),
        //0x0F =>

        0x11 => Instruction("LDDEd16", Box::new(|vm : &mut Vm| i_ldr16d16(vm, Register::D, Register::E))),
/*dispatch 0x12 = trace "LDDEma"   $ iLDHL lDEm a
dispatch 0x13 = trace "INCDE"    $ iINCr16 lDE
dispatch 0x14 = trace "INCd"     $ iINC d
dispatch 0x15 = trace "DECd"     $ iDEC d
dispatch 0x16 = trace "LDdd8"    $ iLDd8 d
dispatch 0x17 = trace "RLa"      $ iRLA
dispatch 0x18 = trace "JRr8"     $ iJR
dispatch 0x1A = trace "LDaDEm"   $ iLDHL a lDEm
dispatch 0x1B = trace "DECDE"    $ iDECr16 lDE
dispatch 0x1C = trace "INCe"     $ iINC e
dispatch 0x1D = trace "DECe"     $ iDEC e
dispatch 0x1E = trace "LDed8"    $ iLDd8 e
dispatch 0x1F = trace "RCa"      $ iRR a*/

        0x40 => Instruction("LDBB", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::B))),
        0x41 => Instruction("LDBC", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::C))),
        0x42 => Instruction("LDBD", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::D))),
        0x43 => Instruction("LDBE", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::E))),
        0x44 => Instruction("LDBH", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::H))),
        0x45 => Instruction("LDBL", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::L))),
        _ => panic!(format!("missing instruction 0x{:2X} !", opcode)),
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

    let sum = hl![vm] as i16 + modificator;
    set_hl!(vm, sum);
    Clock { m:1, t:8 }
}

/// Implementation for LD[I|D] A (HL)
pub fn i_ldmod_ahl(vm : &mut Vm, modificator : i16) -> Clock {
    reg![vm ; Register::A] = mmu::rb(hl![vm], &mut vm.mmu);

    let sum = hl![vm] as i16 + modificator;
    set_hl!(vm, sum);
    Clock { m:1, t:8 }
}

/// Load the value of A in (HL) and increment HL
///
/// > LDI (HL+) <- A
pub fn i_ldihla(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, 1)}
/// Load the value of (HL) in A and increment HL
///
/// > LDI A <- (HL+)
pub fn i_ldiahl(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, 1)}
/// Load the value of A in (HL) and decrement HL
///
/// > LDD (HL-) <- A
pub fn i_lddhla(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, -1)}
/// Load the value of (HL) in A and decrement HL
///
/// > LDD A <- (HL-)
pub fn i_lddahl(vm : &mut Vm) -> Clock {i_ldmod_hla(vm, -1)}

/// LD Register <- immediate Word8
pub fn i_ldrd8(vm : &mut Vm, dst : Register) -> Clock {
    reg![vm ; dst] = read_program_byte(vm);
    Clock { m:2, t:8 }
}

/// LD (HL) <- immediate Word8
pub fn i_ldhld8(vm : &mut Vm) -> Clock {
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

/// Implement xoring the register A with the value src_val
pub fn i_xor_imp(src_val : u8, vm : &mut Vm) {
    reg![vm ; Register::A] ^= src_val;
    let result = reg![vm ; Register::A];
    set_flag(vm, Flag::Z, result == 0);
}

/// XOR the register A with a register src into A
/// Syntax : `XOR src:Register`
pub fn i_xorrr(vm : &mut Vm, src : Register) -> Clock {
    reset_flags(vm);
    i_xor_imp(reg![vm ; src], vm);
    Clock { m:1, t:8 }
}

/// XOR the register A with (HL) into A
/// Syntax : `XORHL`
pub fn i_xorhl(vm : &mut Vm) -> Clock {
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
/// Syntax : `ORHL`
pub fn i_orhl(vm : &mut Vm) -> Clock {
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
    reg![vm ; reg] += 1;
    let final_val = reg![vm ; reg];
    i_inc_impl(vm, initial_val, final_val);
    Clock { m:1, t:4 }
}

/// Increment (HL), and set Z, H as expected.
/// Always set N to 0.
///
/// Syntax : `INCHL`
pub fn i_inchl(vm : &mut Vm) -> Clock {
    let initial_val = mmu::rb(hl![vm], &vm.mmu);
    let final_val = initial_val + 1;
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
    let final_val = initial_val + 1;
    set_r16(vm, h, l, final_val);

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
    reg![vm ; reg] += 1;
    let final_val = reg![vm ; reg];
    i_dec_impl(vm, initial_val, final_val);
    Clock { m:1, t:4 }
}

/// Decrement (HL), and set Z, H as expected.
/// Always set N to 0.
///
/// Syntax : `INCHL`
pub fn i_dechl(vm : &mut Vm) -> Clock {
    let initial_val = mmu::rb(hl![vm], &vm.mmu);
    let final_val = initial_val + 1;
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
    let final_val = initial_val + 1;
    set_r16(vm, h, l, final_val);

    Clock { m:1, t:8 }
}
