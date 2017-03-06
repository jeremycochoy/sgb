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
