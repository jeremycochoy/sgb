extern crate sgb;

#[test]
fn w_uncombine() {
    let (h, l) = sgb::tools::w_uncombine(0x0A0F);
    assert!(h == 0x0A);
    assert!(l == 0x0F);
}

#[test]
fn w_combine() {
    let v = sgb::tools::w_combine(0x0A, 0x0F);
    assert!(v == 0x0A0F);
}

#[test]
fn w_swap() {
    let v = sgb::tools::swap(0x9C);
    assert!(v == 0xC9);
}

#[test]
fn empty_memory() {
    let v = sgb::tools::empty_memory((0..3));
    assert!(vec![0u8, 0, 0] == v);
    let w = sgb::tools::empty_memory((7..10));
    assert!(vec![0u8, 0, 0] == w);
}
