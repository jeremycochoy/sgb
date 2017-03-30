/** IO Module (Contain all the IO access at FF00-FF7F

Export functions reading / writing to the GPU registers,
joypad flags...

Called by the MMU module.
 */

use vm::*;
use gpu::*;
use mmu::*;

pub fn dispatch_io_read(addr : usize, vm : &Vm) -> u8 {
    // TODO Check if io are allowed
    // depending of the state of gpu.gpu_mode:GpuMode.
    match addr {
        0xFF04 => vm.cpu.timers.div,
        0xFF05 => vm.cpu.timers.tima,
        0xFF06 => vm.cpu.timers.tma,
        0xFF40 => lcdc_to_u8(vm.gpu.lcdc),
        0xFF42 => vm.gpu.scy,
        0xFF43 => vm.gpu.scx,
        0xFF44 => vm.gpu.line,
        0xFF47 => vm.gpu.bg_palette,
        0xFF48 => vm.gpu.obj_palette_0,
        0xFF49 => vm.gpu.obj_palette_1,
        0xFF00 => read_joypad(vm),
        0xFF0F => interrupt_to_u8(vm.mmu.ifr),
        0xFFFF => interrupt_to_u8(vm.mmu.ier),
        _ => {println!("Unimplemented read at {:04X}", addr); 0}, //TODO
    }
}

pub fn dispatch_io_write(addr : usize, value :u8, vm : &mut Vm) {
    // TODO Check if io are allowed
    // depending of the state of gpu.gpu_mode:GpuMode.
    match addr {
        0xFF04 => vm.cpu.timers.div = 0,
        0xFF05 => vm.cpu.timers.tima = value, // TODO: expected behavior = ?
        0xFF06 => vm.cpu.timers.tma = value,
        0xFF40 => vm.gpu.lcdc = u8_to_lcdc(value),
        0xFF42 => vm.gpu.scy = value,
        0xFF43 => vm.gpu.scx = value,
        0xFF44 => vm.gpu.line = 0,
        0xFF46 => dma(vm, value),
        0xFF47 => vm.gpu.bg_palette = value,
        0xFF48 => vm.gpu.obj_palette_0 = value,
        0xFF49 => vm.gpu.obj_palette_1 = value,
        0xFF00 => write_joypad(vm, value),
        0xFF0F => vm.mmu.ifr = u8_to_interrupt(value),
        0xFFFF => vm.mmu.ier = u8_to_interrupt(value),
        _ => println!("Unimplemented write at {:04X}", addr), //TODO
    }
}

pub fn read_joypad(vm : &Vm) -> u8 {
    if vm.mmu.joyp & 0x30 == 0x10 {
        return vm.joypad_row_buttons | 0x10;
    }
    if vm.mmu.joyp & 0x30 == 0x20 {
        return vm.joypad_row_cross | 0x20;
    }
    if vm.mmu.joyp & 0x30 == 0x00 {
        return vm.joypad_row_buttons & vm.joypad_row_cross;
    }

    return 0;
}

pub fn write_joypad(vm : &mut Vm, value : u8) {
    vm.mmu.joyp = (value & 0x30) | (vm.mmu.joyp & 0x0F);
}

pub fn dma(vm : &mut Vm, value : u8) {
    // Compute the address value:00
    let addr = (value as u16) << 8;

    // Copy each sprite
    for i in 0..(40 * 4) {
        let byte = rb(addr + i, vm);
        wb(0xFE00 + i, byte, vm);
    }
}
