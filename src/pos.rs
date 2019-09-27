///
/// A position struct
///

#[derive(PartialEq, Debug)]
pub struct Pos{
    pub line: u32,
    pub col: u32
}

impl Pos{

    pub fn new(line: u32, col: u32) -> Pos{
        Pos{line, col}
    }
    
}
