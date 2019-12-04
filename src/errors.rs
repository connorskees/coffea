use std::fmt;
use std::io;

pub type JResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    IoError(io::Error),
    IndexError(u32),
    /// Failed to convert number while parsing
    NumberConversionError(std::num::TryFromIntError),
    MethodNotFound,
    EmptyStack,
}


impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseError::IoError(err) => write!(f, "{:?} {}", self, err),
            ParseError::IndexError(line) => write!(f, "{:?} line: {}", self, line),
            ParseError::NumberConversionError(err) => write!(f, "{:?} {}", self, err),
            ParseError::MethodNotFound => write!(f, "{:?}", self),
            ParseError::EmptyStack => write!(f, "{:?}", self),
        }
    }
}

impl std::error::Error for ParseError {}


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
