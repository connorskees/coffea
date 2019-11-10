use std::io::prelude::*;
use std::io::{self, Read, BufRead, BufReader};
use std::fs::File;

const TEST_CLASS_FILE_PATH: &str = "test.class";

/// Read `n` bytes as [u8; n] 
macro_rules! read_bytes_to_buffer {
    ($reader:ident, $bytes:literal) => {
        if let Some(mut buffer) = Some([0u8; $bytes]) {
            $reader.read_exact(&mut buffer)?;
            // u32::from_le_bytes(buffer).to_be_bytes()
            buffer
        } else {
            unreachable!()
        }
    };
}

/// Read a single byte as a u8
macro_rules! read_u8 {
    ($reader:ident) => {
        if let Some(mut buffer) = Some([0u8]) {
            $reader.read_exact(&mut buffer)?;
            u8::from_le_bytes(buffer)
        } else {
            unreachable!()
        }
    };
}

/// Read 2 bytes as a u16
macro_rules! read_u16 {
    ($reader:ident) => {
        if let Some(mut buffer) = Some([0u8; 2]) {
            $reader.read_exact(&mut buffer)?;
            u16::from_le_bytes(buffer)
        } else {
            unreachable!()
        }
    };
}

/// Read 4 bytes as a u16
macro_rules! read_u32 {
    ($reader:ident) => {
        if let Some(mut buffer) = Some([0u8; 4]) {
            $reader.read_exact(&mut buffer)?;
            u32::from_le_bytes(buffer)
        } else {
            unreachable!()
        }
    };
}

fn main() -> io::Result<()> {
    let mut reader = BufReader::new(File::open(TEST_CLASS_FILE_PATH)?);
    let magic_number = read_u32!(reader);
    assert_eq!(magic_number, 0xCAFEBABE);

    dbg!(magic_number);

    Ok(())
}
