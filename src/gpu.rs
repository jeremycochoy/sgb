use tools::*;
use cpu::*;

#[derive(PartialEq, Eq, Debug)]
/// Represent the memory, registers and flags of the GPU
pub struct Gpu {
    /// Clock used to switch mode
    pub clock           : Clock,
    /// Current mode of the GPU
    pub mode            : GpuMode,
    /// Number of the current line
    pub line            : u16,
    /// Scroll X register
    pub scx             : u8,
    /// Scroll Y register
    pub scy             : u8,
    /// Background Palette
    pub bg_palette      : u8,
    /// LCDC register
    pub lcdc            : u8,
    /// Memory used for rendering the current screen
    pub rendering_memory        : Vec<u8>,
}

impl Default for Gpu {
    fn default() -> Gpu {
        Gpu {
            clock       : Default::default(),
            mode        : GpuMode::HorizontalBlank,
            line        : 0,
            scx         : 0,
            scy         : 0,
            bg_palette  : 0, // TODO
            lcdc        : 0x91, // TODO
            rendering_memory    : empty_memory(0..144*160*3),
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

