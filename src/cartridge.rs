use std::fs::File;
use std::io::{Read, Result, Error, ErrorKind};
use mmu::*;

// Game boy color flag
pub enum CGBFlag {
    CGBOnly,
    CGBCompat,
    CGBOff,
}

pub enum SGBFlag {
    SGBOn,
    SCGBOff,
}

#[derive(PartialEq, Eq, Debug)]
pub enum MBCType {
    ROM,
    MBC1,
    MBC2,
    MBC3,
    MBC4,
    MBC5,
    MMM01,
}

#[derive(PartialEq, Eq, Debug)]
pub enum CartridgeType {
    Cartridge {
        mbc_type : MBCType,
        ram      : bool,
        battery  : bool,
        timer    : bool,
        rumble   : bool
    },
    PocketCamera,
    Tama5,
    HuC3,
    HuC1,
}

impl Default for CartridgeType {
  fn default() -> CartridgeType { CartridgeType::Cartridge {
      mbc_type : MBCType::ROM,
      ram      : false,
      battery  : false,
      timer    : false,
      rumble   : false,
  }
  }
}

// Describe a cartridge
#[derive(PartialEq, Eq, Default, Debug)]
pub struct CartridgeDesc {
    title : String,
    manufacturer : String,
    cartridge_type : CartridgeType,
}

pub fn get_cartridge_type(byte : u8) -> Option<CartridgeType> {
    let mut def = Default::default();
    match def {
        CartridgeType::Cartridge {
            ref mut mbc_type,
            ref mut ram,
            ref mut battery,
            ref mut timer,
            ref mut rumble
        } =>
            match byte {

                0x00 => {*mbc_type = MBCType::ROM},
                0x01 => {*mbc_type = MBCType::MBC1},
                0x02 => {*mbc_type = MBCType::MBC1; *ram = true},
                0x03 => {*mbc_type = MBCType::MBC1; *ram = true; *battery = true},

                0x05 => {*mbc_type = MBCType::MBC2},
                0x06 => {*mbc_type = MBCType::MBC2; *battery = true},

                0x08 => {*mbc_type = MBCType::ROM; *ram = true},
                0x09 => {*mbc_type = MBCType::ROM; *ram = true; *battery = true},

                0x0B => {*mbc_type = MBCType::MMM01},
                0x0C => {*mbc_type = MBCType::MMM01; *ram = true}
                0x0D => {*mbc_type = MBCType::MMM01; *ram = true; *battery = true}

                0x0F => {*mbc_type = MBCType::MBC3; *timer = true; *battery = true}
                0x10 => {*mbc_type = MBCType::MBC3; *timer = true; *ram = true; *battery = true}
                0x11 => {*mbc_type = MBCType::MBC3}
                0x12 => {*mbc_type = MBCType::MBC3; *ram = true}
                0x13 => {*mbc_type = MBCType::MBC3; *ram = true; *battery = true}

                0x15 => {*mbc_type = MBCType::MBC4}
                0x16 => {*mbc_type = MBCType::MBC4; *ram = true}
                0x17 => {*mbc_type = MBCType::MBC4; *ram = true; *battery = true}

                0x19 => {*mbc_type = MBCType::MBC5}
                0x1A => {*mbc_type = MBCType::MBC5; *ram = true}
                0x1B => {*mbc_type = MBCType::MBC5; *ram = true; *battery = true}
                0x1C => {*mbc_type = MBCType::MBC5; *rumble = true}
                0x1D => {*mbc_type = MBCType::MBC5; *rumble = true; *ram = true}
                0x1E => {*mbc_type = MBCType::MBC5; *rumble = true; *ram = true; *battery = true}

                0xFC => return Some(CartridgeType::PocketCamera),
                0xFD => return Some(CartridgeType::Tama5),
                0xFE => return Some(CartridgeType::HuC3),
                0xFF => return Some(CartridgeType::HuC1),
                _ => return None,
            },
        _ => panic!("Default cartridge should be of type CartridgeType::Cartridge"),
    }
    Some(def)
}

pub fn mmu_from_rom_file(filename : String) -> Result<Mmu> {
    let mut file = try!(File::open(filename));

    let mut contents : Vec<u8> = Vec::new();

    let number_of_bytes = try!(file.read_to_end(&mut contents));

    match number_of_bytes {
        0x8000 => {
            let mmu = Mmu {
                rom : contents[0x0000..0x4000].to_vec(),
                srom : contents[0x4000..0x8000].to_vec(),
                .. Default::default()
            };
            return Ok(mmu);
        }
        _ => return Err(Error::new(ErrorKind::Other, "Wrong file size"))
    }
}
