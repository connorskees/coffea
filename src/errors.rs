use std::io;

#[derive(Debug)]
pub enum ParseError {
    IoError(io::Error),
    IndexError,
    MethodNotFound
}

impl From<io::Error> for ParseError {
    fn from(error: io::Error) -> Self {
        ParseError::IoError(error)
    }
}
