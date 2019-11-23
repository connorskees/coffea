#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MajorVersion {
    JavaSE14 = 58,
    JavaSE13 = 57,
    JavaSE12 = 56,
    JavaSE11 = 55,
    JavaSE10 = 54,
    JavaSE9 = 53,
    JavaSE8 = 52,
    JavaSE7 = 51,
    JavaSE6 = 50,
    JavaSE5 = 49,
    JDK1_4 = 48,
    JDK1_3 = 47,
    JDK1_2 = 46,
    JDK1_1 = 45,
}

impl MajorVersion {
    pub fn from_u16(n: u16) -> MajorVersion {
        match n {
            58 => MajorVersion::JavaSE14,
            57 => MajorVersion::JavaSE13,
            56 => MajorVersion::JavaSE12,
            55 => MajorVersion::JavaSE11,
            54 => MajorVersion::JavaSE10,
            53 => MajorVersion::JavaSE9,
            52 => MajorVersion::JavaSE8,
            51 => MajorVersion::JavaSE7,
            50 => MajorVersion::JavaSE6,
            49 => MajorVersion::JavaSE5,
            48 => MajorVersion::JDK1_4,
            47 => MajorVersion::JDK1_3,
            46 => MajorVersion::JDK1_2,
            45 => MajorVersion::JDK1_1,
            _ => unimplemented!(),
        }
    }
}
