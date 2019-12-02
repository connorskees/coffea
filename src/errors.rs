use std::io;

pub type JResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    IoError(io::Error),
    IndexError(u32),
    /// Failed to convert number while parsing
    NumberConversionError(std::num::TryFromIntError),
    MethodNotFound,
}

impl From<io::Error> for ParseError {
    fn from(error: io::Error) -> Self {
        ParseError::IoError(error)
    }
}

impl From<std::num::TryFromIntError> for ParseError {
    fn from(error: std::num::TryFromIntError) -> Self {
        ParseError::NumberConversionError(error)
    }
}
