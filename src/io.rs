/** IO Module (Contain all the IO access at FF00-FF7F

Export functions reading / writing to the GPU registers,
joypad flags...

Called by the MMU module.
 */

use vm::*;
use gpu::*;

pub fn dispatch_io_read(addr : usize, vm : &Vm) -> u8 {
    // TODO Check if io are allowed
    // depending of the state of gpu.gpu_mode:GpuMode.
    match addr {
        0xFF40 => lcdc_to_u8(vm.gpu.lcdc),
        0xFF42 => vm.gpu.scy,
        0xFF43 => vm.gpu.scx,
        0xFF44 => vm.gpu.line,
        0xFF47 => vm.gpu.bg_palette,
        0xFF00 => read_joypad(vm),
        _ => 0, //TODO
    }
}

pub fn dispatch_io_write(addr : usize, value :u8, vm : &mut Vm) {
    // TODO Check if io are allowed
    // depending of the state of gpu.gpu_mode:GpuMode.
    match addr {
        0xFF40 => vm.gpu.lcdc = u8_to_lcdc(value),
        0xFF42 => vm.gpu.scy = value,
        0xFF43 => vm.gpu.scx = value,
        0xFF44 => vm.gpu.line = value,
        0xFF47 => vm.gpu.bg_palette = value,
        0xFF00 => write_joypad(vm, value),
        _ => return, //TODO
    }
}

pub fn read_joypad(vm : &Vm) -> u8 {
    if vm.mmu.joyp & 0x30 == 0x10 {
        println!("JOYPAD 0x10: {:02X}", vm.joypad_row_buttons | 0x10);
        return vm.joypad_row_buttons | 0x10;
    }
    if vm.mmu.joyp & 0x30 == 0x20 {
        println!("JOYPAD 0x20: {:02X}", vm.joypad_row_cross | 0x20);
        return vm.joypad_row_cross | 0x20;
    }
    if vm.mmu.joyp & 0x30 == 0x30 {
        println!("JOYPAD 0x30: {:02X}", vm.joypad_row_buttons | vm.joypad_row_cross | 0x30);
        return vm.joypad_row_buttons | vm.joypad_row_cross | 0x30;
    }

    return 0;
}

pub fn write_joypad(vm : &mut Vm, value : u8) {
    println!("Write joypad: {:02X}", value);
    vm.mmu.joyp = (value & 0x30) | (vm.mmu.joyp & 0x0F);
}
