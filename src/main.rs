extern crate sgb;
extern crate sdl2;

use sgb::*;
use vm;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::*;
use std::env;

pub fn render_screen(vm : &mut Vm, renderer : &mut Renderer, texture : &mut Texture) {
    // Copy the rendering memory of the VM onto the texture
    texture.with_lock(None, |buffer: &mut [u8], pitch: usize| {
        for x in 0..160 {
            for y in 0..144 {
                let offset = y*pitch + x*3;
                buffer[offset] = vm.gpu.rendering_memory[(y*160 + x)*3];
                buffer[offset + 1] = vm.gpu.rendering_memory[(y*160 + x)*3 + 1];
                buffer[offset + 2] = vm.gpu.rendering_memory[(y*160 + x)*3 + 2];
            }
        }
    }).unwrap();

    // Blit the texture on screen and display it
    renderer.copy(&texture, None, Some(Rect::new(0, 0, 160, 144))).unwrap();
    renderer.present();
}

// Main function used for profiling
pub fn main_perf() {
    // Reag arguments (file and remove boot)
    let mut file_name = "".to_string();
    let mut no_boot_rom = false;
    for arg in env::args().skip(1) {
        if arg == "--no-boot" || arg == "--no-boot-rom" {
            no_boot_rom = true;
            println!("Boot rom removed");
        }
        else {
            file_name = arg;
        }
    }
    if file_name == "" {
        println!("usage : sgb file_name.gb");
        return;
    } else {
        println!("Loading: {}", file_name);
    }

    // Load the VM
    let mut vm = load_rom(file_name).unwrap();
    // Disable bios if asked
    if no_boot_rom {
        vm.cpu.registers.pc = 0x100;
        vm.mmu.bios_enabled = false;
    }

    //DEBUG
    while(vm.cpu.clock.t <= 4190000 * 30) {
        execute_one_instruction(&mut vm);
    }
}

pub fn main() { main_shell(); }

pub fn main_shell() {
    // Initialise SDL2
    let sdl_context = sdl2::init().unwrap();
    let video_subsystem = sdl_context.video().unwrap();

    let window = video_subsystem.window("SGB Shield", 160, 144)
        .position_centered()
        .opengl()
        .build()
        .unwrap();

    // Initialise rendering stuff
    let mut renderer = window.renderer().build().unwrap();
    let mut texture = renderer.create_texture_streaming(
        PixelFormatEnum::RGB24, 160, 144).unwrap();
    renderer.clear();
    renderer.present();

    // Reag arguments (file and remove boot)
    let mut file_name = "".to_string();
    let mut no_boot_rom = false;
    for arg in env::args().skip(1) {
        if arg == "--no-boot" || arg == "--no-boot-rom" {
            no_boot_rom = true;
            println!("Boot rom removed");
        }
        else {
            file_name = arg;
        }
    }
    if file_name == "" {
        println!("usage : sgb file_name.gb");
        return;
    } else {
        println!("Loading: {}", file_name);
    }

    // Load the VM
    let mut vm = load_rom(file_name).unwrap();
    // Disable bios if asked
    if no_boot_rom {
        vm.cpu.registers.pc = 0x100;
        vm.mmu.bios_enabled = false;
    }

    // Event Loop
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..}
                | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },

                //
                // Implement keydown for joypad
                //
                Event::KeyDown { keycode: Some(keycode), ..} => {
                    match keycode {
                        Keycode::Z => vm::press_a(&mut vm),
                        Keycode::X => vm::press_b(&mut vm),
                        Keycode::A => vm::press_start(&mut vm),
                        Keycode::S => vm::press_select(&mut vm),

                        Keycode::Down => vm::press_down(&mut vm),
                        Keycode::Up => vm::press_up(&mut vm),
                        Keycode::Left => vm::press_left(&mut vm),
                        Keycode::Right => vm::press_right(&mut vm),

                        _ => (),
                    }
                }

                //
                // Implement keyup for joypad
                //
                Event::KeyUp { keycode: Some(keycode), ..} => {
                    match keycode {
                        Keycode::Z => vm::release_a(&mut vm),
                        Keycode::X => vm::release_b(&mut vm),
                        Keycode::A => vm::release_start(&mut vm),
                        Keycode::S => vm::release_select(&mut vm),

                        Keycode::Down => vm::release_down(&mut vm),
                        Keycode::Up => vm::release_up(&mut vm),
                        Keycode::Left => vm::release_left(&mut vm),
                        Keycode::Right => vm::release_right(&mut vm),

                        _ =>  (),
                    }
                }

                _ => {}
            }
        }

        // Run some instructions
        for i in 0..100 {
            execute_one_instruction(&mut vm);
        }

        // Render screen if we are during vblank period
        // (we don't want to render too often because it would slow down
        // the whole emulator)
        if (vm.gpu.mode == GpuMode::HorizontalBlank) {
            render_screen(&mut vm, &mut renderer, &mut texture);
            while (vm.gpu.mode == GpuMode::HorizontalBlank) {
                execute_one_instruction(&mut vm);
            }
        }
    }
}
