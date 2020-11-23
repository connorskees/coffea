use std::{
    fs::File,
    io::{stdout, BufReader, BufWriter},
};

use coffea::{ClassFile, JResult};

fn main() -> JResult<()> {
    let out = std::process::Command::new("javac")
        .args(&["test.java"])
        .output()
        .unwrap();

    if !out.stderr.is_empty() {
        dbg!(out);
        std::process::exit(1);
    }

    let reader = BufReader::new(File::open("test.class")?);
    let file = ClassFile::from_bufreader(reader)?;

    // let mut outfile = File::create("testout.java")?;
    let outfile = BufWriter::new(stdout());

    file.print(outfile)?;
    Ok(())
}
