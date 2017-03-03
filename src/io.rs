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
        _ => return, //TODO
    }
}
