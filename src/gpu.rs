use tools::*;
use vm::*;
use mmu;

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
    /// LCDC register
    pub lcdc            : LCDC,
    /// Memory used for rendering the current screen
    pub rendering_memory        : Vec<u8>,
}

impl Default for Gpu {
    fn default() -> Gpu {
        Gpu {
            clock       : Default::default(),
            mode        : GpuMode::ScanlineOAM,
            line        : 0,
            scx         : 0,
            scy         : 0,
            bg_palette  : 0, // TODO
            lcdc        : u8_to_lcdc(0x91),
            rendering_memory    : white_memory(0..144*160*3),
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
    /// Bit 4 - BG & Window Tile Data Select
    ///   0: $8800-$97FF
    ///   1: $8000-$8FFF <- Same area as OBJ
    tile_set            : bool,
    /// Bit 3 - BG Tile Map Display Select
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
    window_display  : bool,
}

pub fn lcdc_to_u8(lcdc : LCDC) -> u8 {
    (lcdc.window_display as u8)
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
        window_display  : (0b00000001 & value) != 0,
        sprite_display  : (0b00000010 & value) != 0,
        sprite_size     : (0b00000100 & value) != 0,
        bg_tile_map     : (0b00001000 & value) != 0,
        tile_set        : (0b00010000 & value) != 0,
        window          : (0b00100000 & value) != 0,
        window_tile_map : (0b01000000 & value) != 0,
        display         : (0b10000000 & value) != 0,
    }
}

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
pub fn get_tile_pixels_line(vram : &Vec<u8>, tile_idx : u8, line_idx : u8) -> Vec<u8> {
    let mut px = Vec::new();
    px.push(2);
    px.push(1);
    px.push(2);
    px.push(2);
    px.push(1);
    px.push(2);
    px.push(1);
    px.push(2);
    return px;
}

/// Load tile map line
///
/// The coordinates `x` and `y` should be the location of the
/// first tile in the 32x32 tile map. The tile map (0 or 1) is
/// selected from `lcdc`'s flag `bg_tile_map`.
///
/// The function return a slice of `(SCREEN_WIDTH/8 + 1)` tile indexes.
/// The slice point directly to the mmu's vram.
///
/// The slice's length is one more tile that can be displayed on a line.
/// This allow to display only a part of the first and last tile (wen scx and
/// scy are not multiples of 8).
pub fn load_tile_map_line<'a>(gpu : &Gpu, vram : &'a Vec<u8>, x : u8, y : u8) -> &'a [u8] {
    let x = x as usize;
    let y = y as usize;
    let addr = if gpu.lcdc.bg_tile_map {0x9C00} else {0x9800};

    // Number of tiles in one line
    let w = SCREEN_WIDTH / 8;

    // Compute a slice of w+1 values on the vram
    let addr_cell = addr + x + y * w - 0x9000;
    return &vram[addr_cell..(addr_cell + w + 1)];
}

/// Render the current line of pixel on the rendering_memory
pub fn render_scanline(vm : &mut Vm) {
    // Compute the coordinates of the upper left corner of the screen
    let x = vm.gpu.scx;
    let y = vm.gpu.scy + vm.gpu.line;

    // Compute the line of tiles
    let map_x = x / 8;
    let map_y = y / 8;
    let tile_line = load_tile_map_line(&vm.gpu, &vm.mmu.vram, map_x, map_y).into_iter();

    // Compute a line of pixels
    let vram = &vm.mmu.vram;
    let pixels_line = tile_line
        .map(|idx| get_tile_pixels_line(vram, *idx, y % 8)) //[tile_index] -> [line of pixels]
        .map(|tile| tile.into_iter().map(u8_to_color)) // [[Pixel]] -> [[GreyScale]]
        .map(|pixels| pixels.map(color_to_rgb)); // [[GreyScale]] -> [(r, g, b)]

    // Update the memory with the line of pixels
    let mut out_addr = (vm.gpu.line as usize) * 160 * 3 - (x as usize) % 8;
    println!("Line:{} start:{}", vm.gpu.line, 0 - (x as isize) % 8);
    for tile in pixels_line {
        for (r, g, b) in tile {
            if out_addr >= 0 && out_addr < ((vm.gpu.line as usize) * 160 + SCREEN_WIDTH) * 3 {
                vm.gpu.rendering_memory[out_addr] = r;
                vm.gpu.rendering_memory[out_addr + 1] = g;
                vm.gpu.rendering_memory[out_addr + 2] = b;
            }
            out_addr += 3;
        }
    }
}

pub enum GreyScale {
    WHITE,
    LIGHTGREY,
    DARKGREY,
    BLACK,
}

// TODO : Use the palette to compute the color
pub fn u8_to_color(value : u8) -> GreyScale {
    match value {
        0 => GreyScale::BLACK,
        1 => GreyScale::LIGHTGREY,
        2 => GreyScale::DARKGREY,
        3 => GreyScale::WHITE,
        _ => panic!("Invalid value in u8_to_color call"),
    }
}

pub fn color_to_rgb(color : GreyScale) -> (u8, u8, u8) {
    match color {
        GreyScale::WHITE        => (0xFF, 0xFF, 0xFF),
        GreyScale::LIGHTGREY    => (0xC0, 0xC0, 0xC0),
        GreyScale::DARKGREY     => (0x60, 0x60, 0x60),
        GreyScale::BLACK        => (0x00, 0x00, 0x00),
    }
}
