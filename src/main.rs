extern crate sgb;

use sgb::*;

fn main() {
    let res = load_rom("space.gb".to_string());
    println!("{:?}", res);
}
