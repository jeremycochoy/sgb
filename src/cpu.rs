use vm::*;
use mmu;

#[derive(PartialEq, Eq, Default, Debug)]
pub struct Registers {
        // Registers :
        a  : u8,
        b  : u8,
        c  : u8,
        d  : u8,
        e  : u8,
        h  : u8,
        l  : u8,
        // Flags :
        f  : u8,
        // Program counter
        pc : u16,
        // Stack pointer
        sp : u16,
}

#[derive(PartialEq, Eq, Default, Debug)]
pub struct Clock {
        m : u32,
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
    vm.cpu.registers.pc += 1;
    mmu::rb(vm.cpu.registers.pc, &vm.mmu)
}

/// Read a word (2bytes) from the memory pointed by PC, and increment PC
pub fn read_program_word(vm : &mut Vm) -> u16 {
    vm.cpu.registers.pc += 2;
    mmu::rw(vm.cpu.registers.pc, &vm.mmu)
}
