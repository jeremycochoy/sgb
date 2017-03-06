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
    reg![vm ; Register::H] = 0x80;
    reg![vm ; Register::L] = 0x80;

    i_ldhlmd8(&mut vm);

    assert!(rb(hl![vm], &vm) == 0xCE);
}
