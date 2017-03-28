use tools::*;
use vm::*;

const SCREEN_WIDTH  : usize = 160;
const SCREEN_HEIGHT : usize = 144;

#[derive(PartialEq, Eq, Debug)]
/// Represent the memory, registers and flags of the GPU
pub struct Gpu {
    /// Clock (in cycles) used to switch mode
    pub clock           : u64,
    /// Current mode of the GPU
    pub mode            : GpuMode,
    /// Number of the current line
    pub line            : u8,
    /// Scroll X register
    pub scx             : u8,
    /// Scroll Y register
    pub scy             : u8,
    /// Background Palette
    pub bg_palette      : u8,
    /// Object Palette 0
    pub obj_palette_0   : u8,
    /// Object Palette 1
    pub obj_palette_1   : u8,
    /// LCDC register
    pub lcdc            : LCDC,
    /// Memory used for rendering the current screen
    pub rendering_memory        : Vec<u8>,
    /// Sprite stored in OAM
    /// (duplicate the values in OAM
    /// with easy access for rendering)
    /// The length of sprites is exatly 40.
    pub sprites         : Box<[Sprite]>,
}

impl Default for Gpu {
    fn default() -> Gpu {
        Gpu {
            clock       : Default::default(),
            mode        : GpuMode::ScanlineOAM,
            line        : 0,
            scx         : 0,
            scy         : 0,
            bg_palette  : 0xFC, // TODO : Check initial values when booting without rom
            obj_palette_0 : 0xFF,
            obj_palette_1 : 0xFF,
            lcdc        : u8_to_lcdc(0x91),
            rendering_memory    : white_memory(0..144*160*3),
            sprites     : Box::new([Default::default(); 40]),
        }
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum GpuMode {
    /// Horizontable blank mode.
    /// Both OAM and VRAM are accessible.
    HorizontalBlank,
    /// First part of the scanline mode.
    /// The OAM is used and unaccessible from
    /// the CPU.
    ScanlineOAM,
    /// Second part of the scanline mode.
    /// Both OAM and VRAM are used by the GPU
    /// and are unaccessible from the CPU.
    ScanlineVRAM,
    /// Vertical blank mode. Both
    /// OAM and VRAM are accessible.
    VerticalBlank,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
/// Represent the diferent colors a GB can display
/// on screen.
pub enum GreyScale {
    WHITE,
    LIGHTGREY,
    DARKGREY,
    BLACK,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
/// The LCDC register. It's default value is 0x91.
pub struct LCDC {
    /// Bit 7 - LCD Control Operation *
    ///   0: Stop completely (no picture on screen)
    ///   1: operation
    display             : bool,
    /// Bit 6 - Window Tile Map Display Select
    ///   0: $9800-$9BFF
    ///   1: $9C00-$9FFF
    window_tile_map     : bool,
    /// Bit 5 - Window Display
    ///   0: off
    ///   1: on
    window              : bool,
    /// Bit 4 - BG & Window tileset selected
    ///   0: $8800-$97FF
    ///   1: $8000-$8FFF <- Same area as OBJ
    tile_set            : bool,
    /// Bit 3 - BG tilemap selected
    ///   0: $9800-$9BFF
    ///   1: $9C00-$9FFF
    bg_tile_map         : bool,
    /// Bit 2 - OBJ (Sprite) Size
    ///   0: 8*8
    ///   1: 8*16 (width*height)
    sprite_size         : bool,
    /// Bit 1 - OBJ (Sprite) Display
    ///   0: off
    ///   1: on
    sprite_display  : bool,
    /// Bit 0 - BG & Window Display
    ///   0: off
    ///   1: on
    background_display  : bool,
}

pub fn lcdc_to_u8(lcdc : LCDC) -> u8 {
    (lcdc.background_display as u8)
        | (lcdc.sprite_display as u8) << 1
        | (lcdc.sprite_size as u8) << 2
        | (lcdc.bg_tile_map as u8) << 3
        | (lcdc.tile_set as u8) << 4
        | (lcdc.window as u8) << 5
        | (lcdc.window_tile_map as u8) << 6
        | (lcdc.display as u8) << 7
}

pub fn u8_to_lcdc(value : u8) -> LCDC {
    LCDC {
        background_display  : (0b00000001 & value) != 0,
        sprite_display  : (0b00000010 & value) != 0,
        sprite_size     : (0b00000100 & value) != 0,
        bg_tile_map     : (0b00001000 & value) != 0,
        tile_set        : (0b00010000 & value) != 0,
        window          : (0b00100000 & value) != 0,
        window_tile_map : (0b01000000 & value) != 0,
        display         : (0b10000000 & value) != 0,
    }
}

#[derive(Eq, PartialEq, Clone, Copy, Debug)]
pub struct Sprite {
    /// Y-coordinate of top-left corner of the sprite
    /// (Value stored is Y minus 16)
    pub y               : isize,
    /// X-coordinate of top-left corner of the sprite
    /// (Value stored is X minus 8)
    pub x               : isize,
    /// Index of the tile in the current tileset
    pub tile_idx        : u8,
    /// Display above background (1) or below (0)
    /// The colour 0 (befor application of the bg_palette)
    /// of the background is replaced by the sprite's pixel.
    pub priority        : bool,
    /// Horizontal flip of the sprite (1:normal, 0:flipped)
    pub y_flip          : bool,
    /// Vertical flip of the sprite (1:normal, 0:flipped)
    pub x_flip          : bool,
    /// Palette selector (palette #0 or palette #1)
    pub palette         : bool,
}

impl Default for Sprite {
    fn default() -> Sprite {
        Sprite {
            y           : -16,
            x           : -8,
            tile_idx    : 0,
            priority    : false,
            y_flip      : false,
            x_flip      : false,
            palette     : false,
        }
    }
}

/// Update the state of the GPU (HorizontalBlank,
/// VerticalBlank, ScanlineOAM, ScanlineVRAM)
/// and call the rendering function render_scanline
/// at the end of each scanline.
pub fn update_gpu_mode(vm : &mut Vm, cycles : u64) {
    // Update the clock
    vm.gpu.clock = vm.gpu.clock.wrapping_add(cycles);

    match vm.gpu.mode {
        GpuMode::HorizontalBlank if vm.gpu.clock >= 204 => {
            vm.gpu.clock -= 204;
            // If it's the last line of the screen
            if vm.gpu.line == 143 {
                vm.gpu.mode = GpuMode::VerticalBlank;
            }
            else {
                vm.gpu.mode = GpuMode::ScanlineOAM;
            }
            vm.gpu.line += 1;
        },
        GpuMode::ScanlineOAM if vm.gpu.clock >= 80 => {
            vm.gpu.clock -= 80;
            vm.gpu.mode = GpuMode::ScanlineVRAM;
        },
        GpuMode::ScanlineVRAM if vm.gpu.clock >= 172 => {
            vm.gpu.clock -= 172;
            vm.gpu.mode = GpuMode::HorizontalBlank;
            render_scanline(vm);
        },
        GpuMode::VerticalBlank if vm.gpu.clock >= 456 => {
            vm.gpu.line += 1;
            // After "10 lines" of wait, go back to scanline
            if vm.gpu.line == 153 {
                vm.gpu.line = 0;
                vm.gpu.mode = GpuMode::ScanlineOAM;
            }
        },
        _ => return,
    }
}

/// Return a line of 8 pixels from a tile
///
/// The index of the tile is given by `tile_idx`.
/// Coordinates `line_idx` is the index of the line,
/// from 0 to 7.
pub fn get_tile_pixels_line(lcdc : LCDC, vram : &Vec<u8>, tile_idx : u8, line_idx : u16) -> Vec<u8> {
    // Each tile contain 8 line. Each line is stored in 2 bytes.
    // Therefor each tile contain 8*2 bytes.
    let addr = if lcdc.tile_set {
        let tile_idx = tile_idx as isize;
        let line_idx = line_idx as isize;
        0x8000 + (tile_idx * 8 + line_idx) * 2 - 0x8000
    } else {
        let tile_idx = (tile_idx as i8) as isize;
        let line_idx = line_idx as isize;
        0x9000 + (tile_idx * 8 + line_idx) * 2 - 0x8000
    } as usize;

    assert!(line_idx <= 7);
    assert!(line_idx >= 0);
    let h = vram[addr];
    let l = vram[addr + 1];

    let mut px = Vec::new();
    for i in 0..8 {
        let h_v = h >> (7 - i) & 0x01;
        let l_v = l >> (7 - i) & 0x01;
        let color = h_v << 1 | l_v;
        assert!(color >= 0 && color <= 3);
        px.push(color);
    }
    return px;
}

/// Load tile map line
///
/// The coordinate `y` should be the number of the line to extract
/// from the 32x32 tile map. The tile map (0 or 1) is
/// selected from `lcdc`'s flag `bg_tile_map`.
///
/// The function return a slice of 32 tile indexes.
/// The slice point directly to the mmu's vram.
///
/// The slice's length is one more tile that can be displayed on a line.
/// This allow to display only a part of the first and last tile (wen scx and
/// scy are not multiples of 8).
pub fn load_tile_map_line<'a>(gpu : &Gpu, vram : &'a Vec<u8>, y : u16) -> &'a [u8] {
    let y = y as usize;
    let addr = if gpu.lcdc.bg_tile_map {0x9C00} else {0x9800};

    // Compute a slice of w+1 values on the vram
    // The number of tiles in one line is 32.
    let addr_cell = addr + y * 32 - 0x8000;
    return &vram[addr_cell..(addr_cell + 32 + 1)];
}


/// Render the background on the screen
///
/// First argument is the address where begin the line of pixel
/// of the rendering buffer.
pub fn render_background(out_addr : isize, vm : &mut Vm) -> Vec<u8> {
    // Compute the coordinates of the upper left corner of the line
    let x = vm.gpu.scx as u16;
    let y = (vm.gpu.scy as u16) + (vm.gpu.line as u16);

    // Compute the vertical wrapping of the line
    let y = y % 256;

    // Alias for easy manipulation
    let vram = &vm.mmu.vram;
    let lcdc = vm.gpu.lcdc;
    let bg_palette = vm.gpu.bg_palette;

    // Compute the line of tiles
    let map_y = y / 8;
    let tile_line = load_tile_map_line(&vm.gpu, vram, map_y);

    // Compute the background's line of pixels
    // and update the rendering memory
    let mut bg_pixel_list : Vec<u8> = Vec::with_capacity(SCREEN_WIDTH + 2);
    let mut out_idx = -((x as isize) % 8);
    let map_x = (x as usize) / 8;
    // For each tile that might cross the screen
    for tile_number in map_x..(map_x + SCREEN_WIDTH / 8 + 2) {

        // For each pixel in the tile (use % 32 for horiwontal wrapping)
        for pixel in get_tile_pixels_line(lcdc, vram, tile_line[tile_number % 32], y % 8) {
            // If the pixel is outside of the screen, skip it
            if out_idx < 0 || out_idx >= (SCREEN_WIDTH as isize) {
                out_idx += 1;
                continue;
            }

            let addr = (out_addr + out_idx * 3) as usize;

            // Store the pixel for sprite rendering
            bg_pixel_list.push(pixel);

            // Compute the color of the pixel using the background palette
            let colored_pixel = compute_u8_from_palette(bg_palette, pixel);
            let color = u8_to_color(colored_pixel);
            let (r, g, b) = color_to_rgb(color);

            // Store the color into the rendering memory
            vm.gpu.rendering_memory[addr] = r;
            vm.gpu.rendering_memory[addr + 1] = g;
            vm.gpu.rendering_memory[addr + 2] = b;

            out_idx += 1;
        }
    }

    return bg_pixel_list;
}

/// Render sprites above/bellow the background
///
/// The first argument is the adress of the begining
/// of the current line in the rendering buffer.
/// The second argument is the line of pixels from the background
/// that is displayed. 0 means transparency.
pub fn render_sprite(out_addr : isize, background_pixels : Vec<u8>, vm : &mut Vm) {
    let lcdc = vm.gpu.lcdc;
    let vram = &vm.mmu.vram;

    // TODO : Sort sprites by X and low addr !
    //        Then keep only the first 10. Cf : GB documentation on sprites.

    // For each sprite of the table
    for i in 0..40 {
        let sprite = vm.gpu.sprites[i];
        let line = vm.gpu.line as isize;
        // If the current line do not intersect the sprite, continue
        if line < sprite.y || line >= sprite.y + 8 { continue; }

        // Select the sprite palette
        let palette = if sprite.palette {
            vm.gpu.obj_palette_1
        } else {
            vm.gpu.obj_palette_0
        };

        // Get the line of pixels
        let pixels = get_tile_pixels_line(lcdc, vram, sprite.tile_idx, if sprite.y_flip {
            7 - (line - sprite.y)
        } else {
            line - sprite.y
        } as u16);

        // Compute the adresse of the pixel line to render
        for i in 0..8 {
            let x = sprite.x as usize + i;

            // Check if the pixel is still in the screen
            if x < 0 {continue};
            if x > SCREEN_WIDTH {continue};
            // Check if the sprite don't have the priority and background
            // isn't transparent, continue.
            if !sprite.priority && background_pixels[x] != 0 {continue};

            let colored_pixel = compute_u8_from_palette(palette, pixels[i]);
            let color = u8_to_color(colored_pixel);
            let (r, g, b) = color_to_rgb(color);

            let addr = (out_addr as usize) + x * 3;
            vm.gpu.rendering_memory[addr] = r;
            vm.gpu.rendering_memory[addr + 1] = g;
            vm.gpu.rendering_memory[addr + 2] = b;
        }
    }
}

/// Render the current line of pixel on the rendering_memory
pub fn render_scanline(vm : &mut Vm) {
    // Compute the adresse of the current line of pixels to render
    let out_addr = (vm.gpu.line as isize) * (SCREEN_WIDTH as isize) * 3;
    let lcdc = vm.gpu.lcdc;

    //
    // BACKGROUND RENDERING
    //

    // Return a list of pixels in the current background line
    let background_pixels = if (lcdc.background_display) {
        render_background(out_addr, vm)
    } else {
        vec![0 ; SCREEN_WIDTH] // Return trensparency if nothing was draw
    };

    //
    // SPRITES RENDERING
    //

    // Check the lcdc flag. If inactivated, skip the rendering.
    if lcdc.sprite_display {
        render_sprite(out_addr, background_pixels, vm);
    }
}

/// Take a tile's pixel `value` (value in [|0, 3|]) and give a color
/// value (value in [|0, 3|]) using `pallette`.
pub fn compute_u8_from_palette(palette : u8, value : u8) -> u8 {
    match value {
        0 => 0x03 & (palette >> 0),
        1 => 0x03 & (palette >> 2),
        2 => 0x03 & (palette >> 4),
        3 => 0x03 & (palette >> 6),
        _ => panic!("Invalid value in u8_to_color call"),
    }
}

/// Take a pixel color `value` and translate it to the GreyScale.
pub fn u8_to_color(value : u8) -> GreyScale {
    match value {
        0 => GreyScale::WHITE,
        1 => GreyScale::LIGHTGREY,
        2 => GreyScale::DARKGREY,
        3 => GreyScale::BLACK,
        _ => panic!("Invalid value in u8_to_color call"),
    }
}

/// Give the rgb colors from a GreyScale
pub fn color_to_rgb(color : GreyScale) -> (u8, u8, u8) {
    match color {
        GreyScale::WHITE        => (0xFF, 0xFF, 0xFF),
        GreyScale::LIGHTGREY    => (0xC0, 0xC0, 0xC0),
        GreyScale::DARKGREY     => (0x60, 0x60, 0x60),
        GreyScale::BLACK        => (0x00, 0x00, 0x00),
    }
}
