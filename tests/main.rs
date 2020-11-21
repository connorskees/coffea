use std::{fs, io::BufReader, process::Command};

use coffea::ClassFile;

macro_rules! test {
    // todo: allow arbitrary compilation args
    // todo: use crc32/md5 to speed up comparison of expected output? (benchmark)
    ($name:ident) => {
        #[test]
        fn $name() {
            let java_file_name = concat!("tests/", stringify!($name), ".java");
            let class_file_name = concat!("tests/", stringify!($name), ".class");
            let out_file_name = concat!("tests/", stringify!($name), ".out");

            let compile_output = Command::new("javac")
                .args(&[java_file_name])
                .output()
                .unwrap();

            // if the file failed to compile, panic
            if !compile_output.stderr.is_empty() {
                dbg!(compile_output);
                panic!();
            }

            let class_file = BufReader::new(fs::File::open(class_file_name).unwrap());
            fs::remove_file(class_file_name).unwrap();
            let class_file = ClassFile::from_bufreader(class_file).unwrap();

            let mut actual_output = Vec::new();

            class_file.print(&mut actual_output).unwrap();
            let expected_output = fs::read_to_string(out_file_name).unwrap();

            assert_eq!(
                unsafe { String::from_utf8_unchecked(actual_output) },
                expected_output
            )
        }
    };
}

test!(instance_variables);
test!(arithmetic);
test!(string_concat_factory);
test!(arrays);
test!(casts);
test!(constants);
