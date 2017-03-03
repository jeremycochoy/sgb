use cpu::*;
use mmu::*;
use gpu::*;
use cartridge::*;

#[derive(PartialEq, Eq, Default, Debug)]
pub struct Vm {
    pub cpu : Cpu,
    pub mmu : Mmu,
    pub gpu : Gpu,
    pub cartridge : CartridgeDesc,
}
