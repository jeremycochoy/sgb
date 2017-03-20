#[macro_use(reg)]
extern crate sgb;

use sgb::*;

#[test]
fn ld_without_dispatch() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    // Write some values in rom
    vm.mmu.rom[0x100] = 0xCE;
    vm.mmu.rom[0x101] = 0x1F;
    vm.mmu.rom[0x102] = 0x73;

    // Run 3 loads
    i_ldrd8(&mut vm, Register::A);
    i_ldrd8(&mut vm, Register::B);
    i_ldrd8(&mut vm, Register::C);

    assert!(reg![vm ; Register::A] == 0xCE);
    assert!(reg![vm ; Register::B] == 0x1F);
    assert!(reg![vm ; Register::C] == 0x73);

    i_ldrr(&mut vm, Register::D, Register::A);
    i_ldrr(&mut vm, Register::E, Register::B);
    i_ldrr(&mut vm, Register::H, Register::C);
    i_ldrr(&mut vm, Register::L, Register::C);

    assert!(reg![vm ; Register::D] == 0xCE);
    assert!(reg![vm ; Register::E] == 0x1F);
    assert!(reg![vm ; Register::H] == 0x73);
    assert!(reg![vm ; Register::L] == 0x73);

    // Move back pc to 0x100
    pc![vm] = 0x100;
    vm.mmu.rom[0x100] = 0x27;
    vm.mmu.rom[0x101] = 0x80;
    vm.mmu.rom[0x102] = 0xCE;

    i_ldr16d16(&mut vm, Register::H, Register::L);
    assert!(hl![vm] == 0x8027);

    i_ldhlmd8(&mut vm);
    assert!(rb(hl![vm], &vm) == 0xCE);

    i_ldr16mr(&mut vm, Register::H, Register::L, Register::B);
    assert!(rb(hl![vm], &vm) == 0x1F);
}


#[test]
fn rra() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;

    reg![vm ; Register::A] = 0b01000101;
    set_flag(&mut vm, Flag::C, false); // carry 0

    // Rotate 0b01001100; and carry 0
    i_rr(&mut vm, Register::A);

    // Check that the rotation is ok
    assert!(reg![vm ; Register::A] == 0b00100010);
    // Check the new state of carry
    assert!(flag![vm ; Flag::C] == true);

    // Rotate again
    i_rr(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 0b10010001);
    assert!(flag![vm ; Flag::C] == false);

    // Rotate so that we go back to the initial state
    i_rr(&mut vm, Register::A);
    i_rr(&mut vm, Register::A);
    i_rr(&mut vm, Register::A);
    i_rr(&mut vm, Register::A);
    i_rr(&mut vm, Register::A);
    i_rr(&mut vm, Register::A);
    i_rr(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 0b01000101);
    assert!(flag![vm ; Flag::C] == false);
}

#[test]
fn rla() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;

    reg![vm ; Register::A] = 0b10000101;
    set_flag(&mut vm, Flag::C, false); // carry 0

    // Rotate 0b01001100; and carry 0
    i_rl(&mut vm, Register::A);

    // Check that the rotation is ok
    assert!(reg![vm ; Register::A] == 0b00001010);
    // Check the new state of carry
    assert!(flag![vm ; Flag::C] == true);

    // Rotate again
    i_rl(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 0b00010101);
    assert!(flag![vm ; Flag::C] == false);

    // Rotate so that we go back to the initial state
    i_rl(&mut vm, Register::A);
    i_rl(&mut vm, Register::A);
    i_rl(&mut vm, Register::A);
    i_rl(&mut vm, Register::A);
    i_rl(&mut vm, Register::A);
    i_rl(&mut vm, Register::A);
    i_rl(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 0b10000101);
    assert!(flag![vm ; Flag::C] == false);
}

#[test]
fn dec() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    reg![vm ; Register::A] = 2;

    i_decr(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 1);
    assert!(flag![vm ; Flag::Z] == false);
    assert!(flag![vm ; Flag::N] == true);

    i_decr(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 0);
    assert!(flag![vm ; Flag::Z] == true);
    assert!(flag![vm ; Flag::N] == true);

    i_decr(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 0xFF);
    assert!(flag![vm ; Flag::Z] == false);
    assert!(flag![vm ; Flag::N] == true);
    assert!(flag![vm ; Flag::H] == true);
}

#[test]
fn adc() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;

    reg![vm ; Register::A] = 3;
    set_flag(&mut vm, Flag::C, true);

    i_adcr(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 7);
    assert!(flag![vm ; Flag::C] == false);

    reg![vm ; Register::B] = 3;
    i_adcr(&mut vm, Register::B);
    assert!(reg![vm ; Register::A] == 10);
    assert!(flag![vm ; Flag::C] == false);

    reg![vm ; Register::A] = 0xFF;
    reg![vm ; Register::B] = 3;
    set_flag(&mut vm, Flag::C, true);
    i_adcr(&mut vm, Register::B);
    assert!(reg![vm ; Register::A] == 3);
}

#[test]
fn push_and_pop() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    // Put some values in BC and push them
    reg![vm ; Register::B] = 0x80;
    reg![vm ; Register::C] = 0x5D;

    i_push(&mut vm, Register::B, Register::C);

    assert!(sp![vm] == 0xFFFE - 2);
    assert!(mmu::rw(sp![vm], &vm) == 0x805D);

    // Put some values in HL and push them
    reg![vm ; Register::H] = 0x10;
    reg![vm ; Register::L] = 0x33;

    i_push(&mut vm, Register::H, Register::L);
    assert!(sp![vm] == 0xFFFE - 4);
    assert!(mmu::rw(sp![vm], &vm) == 0x1033);

    // Pop the HL's values into AF
    i_pop(&mut vm, Register::A, Register::F);
    assert!(sp![vm] == 0xFFFE - 2);
    assert!(reg![vm ; Register::A] == 0x10);
    assert!(reg![vm ; Register::F] == 0x33 & 0xF0);

    // Pop the HL's values into AF
    i_pop(&mut vm, Register::D, Register::E);
    assert!(sp![vm] == 0xFFFE);
    assert!(reg![vm ; Register::D] == 0x80);
    assert!(reg![vm ; Register::E] == 0x5D);
}

#[test]
fn call_ret() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    // Write 0x0200 in little endian
    vm.mmu.rom[0x100] = 0x00;
    vm.mmu.rom[0x101] = 0x02;

    i_call(&mut vm);

    // Check state of PC and SP
    assert!(sp![vm] == 0xFFFE - 2);
    assert!(pc![vm] == 0x0200);
    // Check that the address of the next instruction
    // on the stack is the right one
    assert!(mmu::rw(sp![vm], &vm) == 0x0102);

    i_ret(&mut vm);

    // Check that we are back at 0x0102
    assert!(sp![vm] == 0xFFFE);
    assert!(pc![vm] == 0x0102);
}

#[test]
fn callf_retf() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    // Write 0x0200 in little endian
    vm.mmu.rom[0x100] = 0x00;
    vm.mmu.rom[0x101] = 0x02;
    vm.mmu.rom[0x102] = 0x00;
    vm.mmu.rom[0x103] = 0x02;

    set_flag(&mut vm, Flag::Z, false);
    i_callf(&mut vm, Flag::Z);

    // Check nothing happened
    assert!(sp![vm] == 0xFFFE);
    assert!(pc![vm] == 0x0102);

    set_flag(&mut vm, Flag::Z, true);
    i_callf(&mut vm, Flag::Z);

    // Check state of PC and SP
    assert!(sp![vm] == 0xFFFE - 2);
    assert!(pc![vm] == 0x0200);
    // Check that the address of the next instruction
    // on the stack is the right one
    assert!(mmu::rw(sp![vm], &vm) == 0x0104);

    set_flag(&mut vm, Flag::Z, false);
    i_retf(&mut vm, Flag::Z);
    assert!(sp![vm] == 0xFFFE - 2);
    assert!(pc![vm] == 0x0200);

    set_flag(&mut vm, Flag::Z, true);
    i_retf(&mut vm, Flag::Z);
    // Check that we are back at 0x0102
    assert!(sp![vm] == 0xFFFE);
    assert!(pc![vm] == 0x0104);
}

#[test]
fn callnf_retnf() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    // Write 0x0200 in little endian
    vm.mmu.rom[0x100] = 0x00;
    vm.mmu.rom[0x101] = 0x02;
    vm.mmu.rom[0x102] = 0x00;
    vm.mmu.rom[0x103] = 0x02;

    set_flag(&mut vm, Flag::C, true);
    i_callnf(&mut vm, Flag::C);

    // Check nothing happened
    assert!(sp![vm] == 0xFFFE);
    assert!(pc![vm] == 0x0102);

    set_flag(&mut vm, Flag::C, false);
    i_callnf(&mut vm, Flag::C);

    // Check state of PC and SP
    assert!(sp![vm] == 0xFFFE - 2);
    assert!(pc![vm] == 0x0200);
    // Check that the address of the next instruction
    // on the stack is the right one
    assert!(mmu::rw(sp![vm], &vm) == 0x0104);

    set_flag(&mut vm, Flag::C, true);
    i_retnf(&mut vm, Flag::C);
    assert!(sp![vm] == 0xFFFE - 2);
    assert!(pc![vm] == 0x0200);

    set_flag(&mut vm, Flag::C, false);
    i_retnf(&mut vm, Flag::C);
    // Check that we are back at 0x0102
    assert!(sp![vm] == 0xFFFE);
    assert!(pc![vm] == 0x0104);
}


#[test]
fn inc_flags() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    reg![vm ; Register::A] = 0;
    set_flag(&mut vm, Flag::C, false);
    i_incr(&mut vm, Register::A);
    assert!(flag![vm ; Flag::C] == false);
    set_flag(&mut vm, Flag::C, true);
    i_incr(&mut vm, Register::A);
    assert!(flag![vm ; Flag::C] == true);

    assert!(flag![vm ; Flag::Z] == false);
    assert!(flag![vm ; Flag::N] == false);
    set_flag(&mut vm, Flag::C, false);
    reg![vm ; Register::A] = 0xFF;
    i_incr(&mut vm, Register::A);
    assert!(flag![vm ; Flag::C] == false);
    assert!(flag![vm ; Flag::Z] == true);
}


#[test]
fn dec_flags() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    reg![vm ; Register::A] = 0x10;
    set_flag(&mut vm, Flag::C, false);
    i_decr(&mut vm, Register::A);
    assert!(flag![vm ; Flag::C] == false);
    set_flag(&mut vm, Flag::C, true);
    i_decr(&mut vm, Register::A);
    assert!(flag![vm ; Flag::C] == true);

    assert!(flag![vm ; Flag::Z] == false);
    assert!(flag![vm ; Flag::N] == true);
    set_flag(&mut vm, Flag::C, false);
    reg![vm ; Register::A] = 0x01;
    i_decr(&mut vm, Register::A);
    assert!(flag![vm ; Flag::C] == false);
    assert!(flag![vm ; Flag::Z] == true);
}


#[test]
fn jmphl() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    // Put some values in HL
    reg![vm ; Register::H] = 0x10;
    reg![vm ; Register::L] = 0x33;

    i_jphl(&mut vm);

    assert!(pc![vm] == 0x1033);
}


#[test]
fn pop_af() {
    let mut vm : Vm = Default::default();

    pc![vm] = 0x100;
    sp![vm] = 0xFFFE;

    // Put some values in HL
    reg![vm ; Register::B] = 0x13;
    reg![vm ; Register::C] = 0xFF;

    i_push(&mut vm, Register::B, Register::C);
    i_pop(&mut vm, Register::A, Register::F);

    assert!(reg![vm ; Register::A] == 0x13);
    assert!(reg![vm ; Register::F] == 0xF0);
}


#[test]
fn rrc() {
    let mut vm : Vm = Default::default();

    reg![vm ; Register::A] = 0b10;
    set_flag(&mut vm, Flag::C, true);

    i_rrc(&mut vm, Register::A);
    assert!(reg![vm ; Register::A] == 0b01);
    assert!(flag![vm ; Flag::C] == false);

    reg![vm ; Register::B] = 0b00000001;
    i_rrc(&mut vm, Register::B);
    assert!(reg![vm ; Register::B] == 0b10000000);
    assert!(flag![vm ; Flag::C] == true);

    reg![vm ; Register::C] = 0b00010101;
    i_rrc(&mut vm, Register::C);
    assert!(reg![vm ; Register::C] == 0b10001010);
    assert!(flag![vm ; Flag::C] == true);

    reg![vm ; Register::D] = 0b11110000;
    i_rrc(&mut vm, Register::D);
    assert!(reg![vm ; Register::D] == 0b01111000);
    assert!(flag![vm ; Flag::C] == false);
}
