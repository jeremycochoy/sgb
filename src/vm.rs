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

    /// Keypad column P14 for Down, Up, Left, Right
    pub joypad_row_cross   : u8,
    /// Keypad column P15 for Start, Select, B, A
    pub joypad_row_buttons : u8,
}

/// Binary mask associated to the line
/// of the key.
///
/// Line P10 : A, Right
/// Line P11 : B, Left
/// Line P12 : Select, Up
/// Line P13 : Start, Down
pub mod joypad {
    pub const DOWN   : u8 = 0x08;
    pub const UP     : u8 = 0x04;
    pub const LEFT   : u8 = 0x02;
    pub const RIGHT  : u8 = 0x01;

    pub const START  : u8 = 0x08;
    pub const SELECT : u8 = 0x04;
    pub const B      : u8 = 0x02;
    pub const A      : u8 = 0x01;
}

pub fn press_down(vm : &mut Vm) {
    vm.joypad_row_cross &= !joypad::DOWN;
}

pub fn press_up(vm : &mut Vm) {
    vm.joypad_row_cross &= !joypad::UP;
}

pub fn press_left(vm : &mut Vm) {
    vm.joypad_row_cross &= !joypad::LEFT;
}

pub fn press_right(vm : &mut Vm) {
    vm.joypad_row_cross &= !joypad::RIGHT;
}

pub fn release_down(vm : &mut Vm) {
    vm.joypad_row_cross |= joypad::DOWN;
}

pub fn release_up(vm : &mut Vm) {
    vm.joypad_row_cross |= joypad::UP;
}

pub fn release_left(vm : &mut Vm) {
    vm.joypad_row_cross |= joypad::LEFT;
}

pub fn release_right(vm : &mut Vm) {
    vm.joypad_row_cross |= joypad::RIGHT;
}

//
// Buttons (Start, Select, B, A)
//

pub fn press_start(vm : &mut Vm) {
    vm.joypad_row_buttons &= !joypad::START;
}

pub fn press_select(vm : &mut Vm) {
    vm.joypad_row_buttons &= !joypad::SELECT;
}

pub fn press_b(vm : &mut Vm) {
    vm.joypad_row_buttons &= !joypad::B;
}

pub fn press_a(vm : &mut Vm) {
    vm.joypad_row_buttons &= !joypad::A;
}

pub fn release_start(vm : &mut Vm) {
    vm.joypad_row_buttons |= joypad::START;
}

pub fn release_select(vm : &mut Vm) {
    vm.joypad_row_buttons |= joypad::SELECT;
}

pub fn release_b(vm : &mut Vm) {
    vm.joypad_row_buttons |= joypad::B;
}

pub fn release_a(vm : &mut Vm) {
    vm.joypad_row_buttons |= joypad::A;
}
