#[derive(PartialEq, Eq, Debug)]
struct Gpu {
        vram : Vec<u8>
}

impl Default for Gpu {
    fn default() -> Gpu { Gpu { vram : empty_memory (0x8000 .. 0x9FFF) } }
}
