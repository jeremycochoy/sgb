#[derive(PartialEq, Eq, Default, Debug)]
// The MMU (memory)
struct Mmu {
        // GB Bios
        bios : Vec<u8>,
        // 0000-3FFF    16KB ROM Bank 00
        rom  : Vec<u8>,
        // 4000-7FFF    16KB ROM Bank 01
        srom : Vec<u8>,
}
