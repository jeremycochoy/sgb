use cpu::*;
use mmu::*;
use cartridge::*;

#[derive(PartialEq, Eq, Default, Debug)]
pub struct Vm {
    pub cpu : Cpu,
    pub mmu : Mmu,
    pub cartridge : CartridgeDesc,
}
