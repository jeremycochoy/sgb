extern crate sgb;

use sgb::*;

fn main() {
    let mut res = sgb::cartridge::mmu_from_rom_file("space.gb".to_string());
    println!("{:?}", res.map(sgb::cartridge::describe_cartridge));
}
