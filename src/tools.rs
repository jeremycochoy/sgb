// Combine two input bytes h and l into a 16bit integer containing h:l
pub fn w_combine(h : u8, l : u8) -> u16 {
        (h as u16) << 8 | (l as u16)
}

// Break the higher and lower part of the input 16bit integer into h:l
pub fn w_uncombine(hl : u16) -> (u8, u8) {
        ((hl >> 8) as u8, hl as u8)
}

// Binary swap the 4 lower bits of the input with the 4 higher bits.
pub fn swap(v : u8) -> u8 {
        (v >> 4) | (v << 4)
}

// Replace the each element of the list by a null byte
pub fn empty_memory<I : Iterator>(range : I) -> Vec<u8> {
    range.map(|_| 0).collect()
}
