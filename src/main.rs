extern crate sgb;

use sgb::*;

fn main() {
    let mut vm = load_rom("space.gb".to_string()).unwrap();

    while vm.cpu.registers.pc < 0x100 {
        execute_one_instruction(&mut vm);
    }

    println!("{:?}", vm.cpu);
}
