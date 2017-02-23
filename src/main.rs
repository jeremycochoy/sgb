extern crate sgb;

use sgb::*;

fn main() {
    let res = sgb::cartridge::mmu_from_rom_file("space.gb".to_string());
    println!("{:?}", res);
}
