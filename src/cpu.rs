use vm::*;
use tools::*;
use mmu;
use std::boxed::Box;

#[derive(PartialEq, Eq, Default, Debug)]
pub struct Registers {
        // Registers (a, b, c, d, e, h, l, f) :
        rs : [u8 ; 8],
        // Program counter
        pc : u16,
        // Stack pointer
        sp : u16,
}

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

macro_rules! set_hl {
    ($vm:expr, $value:expr) => {{
        let (h, l) = w_uncombine($value as u16);
        reg![$vm ; Register::H] = h;
        reg![$vm ; Register::L] = l;
    }}
}

#[derive(PartialEq, Eq, Default, Debug)]
/// Represent a 'time' enlapsed
pub struct Clock {
    /// Length in byte of the last instruction
    m : u32,
    /// Duration in cycles
    t : u32,
}

#[derive(PartialEq, Eq, Debug)]
pub enum InterruptState {
        IEnabled,
        IDisabled,
        INextInstD,
        INextInstE,
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


//pub struct Instruction(&'static str, fn(&mut Vm) -> Clock);
pub struct Instruction(&'static str, Box<Fn(&mut Vm) -> Clock>);

pub fn dispatch(opcode : u8) -> Instruction {
    match opcode {
        0x00 => Instruction("NOP", Box::new(i_nop)),

        0x40 => Instruction("LDbb", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::B))),
        0x41 => Instruction("LDbc", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::C))),
        0x42 => Instruction("LDbd", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::D))),
        0x43 => Instruction("LDbe", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::E))),
        0x44 => Instruction("LDbh", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::H))),
        0x45 => Instruction("LDbl", Box::new(|vm : &mut Vm| i_ldrr(vm, Register::B, Register::L))),
        _ => panic!("Missing instruction !"),
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

/// Same as LD, but alow to use (HL) on the right side
///
/// Syntax : `LDrhl vm:Vm dst:Register`
///
/// > LDrHL Register <- (HL)
pub fn i_ldrhl(vm : &mut Vm, dst : Register) -> Clock {
    reg![vm ; dst] = mmu::rb(hl![vm], &vm.mmu);
    Clock { m:1, t:8 }
}

/// Same as LD, but alow to use (HL) on the right side
///
/// Syntax : `LDhlr vm:Vm src:Register`
///
/// > LDrHL (HL) <- Register
pub fn i_ldhlr(vm : &mut Vm, src : Register) -> Clock {
     mmu::wb(hl![vm], reg![vm ; src], &mut vm.mmu);
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

/// LD (a16) <- a where a16 means the next Word16 as an address.
pub fn i_lda16a(vm : &mut Vm) -> Clock {
    let a16 = read_program_word(vm);
    mmu::wb(a16, reg![vm ; Register::A], &mut vm.mmu);
    Clock { m:3, t:12 }
}

/// LD a <- (a16) where a16 means the next Word16 as an address.
pub fn i_ldaa16(vm : &mut Vm) -> Clock {
    let a16 = read_program_word(vm);
    reg![vm ; Register::A] = mmu::rb(a16, &vm.mmu);
    Clock { m:3, t:12 }
}

/// LD (a8) <- a where a8 means the next Word8 + 0xFF00 as an address.
pub fn i_lda8a(vm : &mut Vm) -> Clock {
    let a8 = read_program_byte(vm);
    mmu::wb(a8 as u16 + 0xFF00, reg![vm ; Register::A], &mut vm.mmu);
    Clock { m:3, t:12 }
}

/// LD a <- (a8) where a8 means the next Word8 + 0xFF00 as an address.
pub fn i_ldaa8(vm : &mut Vm) -> Clock {
    let a8 = read_program_byte(vm);
    reg![vm ; Register::A] = mmu::rb(a8 as u16 + 0xFF00, &vm.mmu);
    Clock { m:3, t:12 }
}
