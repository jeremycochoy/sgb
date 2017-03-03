extern crate sgb;
extern crate sdl2;

use sgb::*;
use sdl2::pixels::PixelFormatEnum;
use sdl2::rect::Rect;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use sdl2::render::*;

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

pub fn main() {
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

    // VM
    let mut vm = load_rom("tetris.gb".to_string()).unwrap();

    // Event Loop
    let mut event_pump = sdl_context.event_pump().unwrap();
    'running: loop {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..}
                | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                    break 'running
                },
                _ => {}
            }
        }

        // Run 100 instructions
        for i in 0..100 {
            execute_one_instruction(&mut vm);
        }
        // Render screen
        render_screen(&mut vm, &mut renderer, &mut texture);
    }
}
