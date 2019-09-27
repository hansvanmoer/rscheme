///
/// The lexer
///

use crate::pos::Pos;

use std::str::Chars;
use std::iter::Peekable;

///
/// An enum defining all lexer errors
///
#[derive(PartialEq, Debug)]
pub enum LexerError{

    ///
    /// An unexpected character was encountered
    ///
    BadChar,

    ///
    /// The end of the stream was encountered unexpectedly
    ///
    BadEnd,

    ///
    /// Bad character name
    ///
    BadCharName,

    ///
    /// Bad character hex value
    ///
    BadCharHex,

    ///
    /// The supplied hex value is not a proper Unicode code point
    ///
    InvalidCharHex
}

///
/// A token produced by the lexer
///
#[derive(PartialEq, Debug)]
pub enum Token{

    ///
    /// A token representing a literal boolean
    ///
    Boolean(bool),

    ///
    /// A token representing a literal character
    ///
    Character(char)
}

///
/// A struct containing lexer state
///
pub struct Lexer<'a>{
    input: Peekable<Chars<'a>>,
    line: u32,
    col: u32
}

impl<'a> Lexer<'a>{

    fn is_hex_char(c: & char) -> bool{
        c.is_digit(16)
    }
    
    fn is_character_name_char(c: & char) -> bool{
        c.is_alphabetic()
    }

    fn is_character_hex_delim_char(c: & char) -> bool{
        !c.is_alphanumeric() && c != & '_'
    }
    
    pub fn new(input: &'a str) -> Lexer<'a> {
        Lexer{input: input.chars().peekable(), line: 1, col: 1}
    }

    pub fn line(& self) -> u32{
        self.line
    }

    pub fn col(& self) -> u32{
        self.col
    }

    pub fn pos(& self) -> Pos{
        Pos::new(self.line, self.col)
    }

    fn reset_pos(& mut self, pos: Pos){
        self.line = pos.line;
        self.col = pos.col;
    }

    fn peek(& mut self) -> Option<& char> {
        self.input.peek()
    }

    fn next(& mut self) -> Option<char> {
        match self.input.next() {
            Some(c) => match c {
                '\n' => {
                    self.line = self.line + 1;
                    self.col = 1;
                    Some(c)
                },
                _ => {
                    self.col = self.col + 1;
                    Some(c)
                }
            }
            None => None
        }
    }
    
    fn next_non_breaking(& mut self) -> Option<char> {
        self.col = self.col + 1;
        self.input.next()
    }

    fn skip_non_breaking(& mut self){
        self.input.next();
        self.col = self.col + 1;
    }

    fn lex_character_name(& mut self, expected_name: & str, result: char) -> Result<Token, LexerError>{
        let pos = self.pos();
        let head = self.next_non_breaking().unwrap();
        
        match self.peek() {
            Some(c) => {
                if !Lexer::is_character_name_char(&c) {
                    return Ok(Token::Character(head));
                }
            }
            None => {
                return Ok(Token::Character(head));
            }
        }
        
        let mut iter = expected_name.chars();
        loop {
            match iter.next() {
                Some(expected) => {
                    match self.next_non_breaking() {
                        Some(c) => {
                            if c != expected {
                                self.reset_pos(pos);
                                return Err(LexerError::BadCharName);
                            }
                        },
                        None => {
                            self.reset_pos(pos);
                            return Err(LexerError::BadCharName);
                        }
                    }
                },
                None => {
                    match self.peek() {
                        Some(delim) => {
                            if Lexer::is_character_name_char(delim) {
                                self.reset_pos(pos);
                                return Err(LexerError::BadCharName);
                            }else{
                                return Ok(Token::Character(result));
                            }
                        },
                        None => {
                            return Ok(Token::Character(result))
                        }
                    }
                }
            }
        }
    }

    fn lex_character_hex(& mut self) -> Result<Token, LexerError> {
        let pos = self.pos();
        self.skip_non_breaking();
        let mut buf = String::new();
        loop {
            match self.peek() {
                Some(c) => {
                    if Lexer::is_hex_char(&c) {
                        buf.push(self.next_non_breaking().unwrap());
                    }else if Lexer::is_character_hex_delim_char(&c) {
                        break;
                    }else{
                        self.reset_pos(pos);
                        return Err(LexerError::BadCharHex);
                    }
                },
                None => {
                    break;
                }
            }
        }

        match u32::from_str_radix(&buf, 16) {
            Ok(val) => {
                if val <= 0xD800 || (val >= 0xE000 && val <= 0x10FFF) {
                    Ok(Token::Character(std::char::from_u32(val).unwrap()))
                }else{
                    self.reset_pos(pos);
                    Err(LexerError::InvalidCharHex)
                }
            },
            Err(_) => {
                self.reset_pos(pos);
                Err(LexerError::BadCharHex)
            }
        }
    }
    
    fn lex_character(& mut self) -> Result<Token, LexerError> {
        self.skip_non_breaking();
        match self.peek() {
            Some(c) => match c {
                'a' => self.lex_character_name("larm", '\u{0007}'),
                'b' => self.lex_character_name("ackspace", '\u{0008}'),
                'd' => self.lex_character_name("elete", '\u{007F}'),
                'e' => self.lex_character_name("sc", '\u{001B}'),
                'l' => self.lex_character_name("inefeed", '\u{000A}'),
                'n' => self.lex_character_name("ewline", '\u{000A}'),
                'p' => self.lex_character_name("age", '\u{000C}'),
                'r' => self.lex_character_name("eturn", '\u{000D}'),
                's' => self.lex_character_name("pace", '\u{0020}'),
                't' => self.lex_character_name("ab", '\u{0009}'),
                'v' => self.lex_character_name("tab", '\u{000B}'),
                'x' => self.lex_character_hex(),
                _ => {
                    Ok(Token::Character(self.next().unwrap()))
                }
            },
            None => Err(LexerError::BadCharName)
        }
    }

    fn lex_boolean_or_character(& mut self) -> Result<Token, LexerError> {
        self.skip_non_breaking();
        match self.peek() {
            Some(& c) => {
                match & c {
                    't' => {
                        self.skip_non_breaking();
                        Ok(Token::Boolean(true))
                    },
                    'f' => {
                        self.skip_non_breaking();
                        Ok(Token::Boolean(false))
                    },
                    '\\' => self.lex_character(),
                    _ => Err(LexerError::BadChar)
                }
            },
            None => Err(LexerError::BadEnd)
        }
    }
    
    pub fn lex(& mut self) -> Result<Option<Token>, LexerError> {
        match self.peek() {
            Some(c) => {
                match c {
                    '#' => Ok(Some(self.lex_boolean_or_character()?)),
                    _ => Err(LexerError::BadChar)
                }
            },
            None => Ok(None)
        }
    }

}

#[cfg(test)]
mod test{

    use super::Lexer;
    use super::LexerError;
    use super::Token;
    use crate::pos::Pos;
    
    #[test]
    fn test_lex_true() {
        let mut lexer = Lexer::new("#t");
        assert_eq!(Token::Boolean(true),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }

    #[test]
    fn test_lex_false() {
        let mut lexer = Lexer::new("#f");
        assert_eq!(Token::Boolean(false),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }

    #[test]
    fn test_bad_boolean() {
        let mut lexer = Lexer::new("#r");
        assert_eq!(LexerError::BadChar, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,2), lexer.pos());      
    }
    
    #[test]
    fn test_character_alarm(){
        let mut lexer = Lexer::new("#\\alarm");
        assert_eq!(Token::Character('\u{0007}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,8), lexer.pos());
    }
    
    #[test]
    fn test_character_backspace(){
        let mut lexer = Lexer::new("#\\backspace");
        assert_eq!(Token::Character('\u{0008}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,12), lexer.pos());
    }

    #[test]
    fn test_character_delete(){
        let mut lexer = Lexer::new("#\\delete");
        assert_eq!(Token::Character('\u{007F}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,9), lexer.pos());
    }

    #[test]
    fn test_character_esc(){
        let mut lexer = Lexer::new("#\\esc");
        assert_eq!(Token::Character('\u{001B}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,6), lexer.pos());
    }

    #[test]
    fn test_character_linefeed(){
        let mut lexer = Lexer::new("#\\linefeed");
        assert_eq!(Token::Character('\u{000A}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,11), lexer.pos());
    }

    #[test]
    fn test_character_newline(){
        let mut lexer = Lexer::new("#\\newline");
        assert_eq!(Token::Character('\u{000A}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,10), lexer.pos());
    }

    #[test]
    fn test_character_page(){
        let mut lexer = Lexer::new("#\\page");
        assert_eq!(Token::Character('\u{000C}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,7), lexer.pos());
    }

    #[test]
    fn test_character_return(){
        let mut lexer = Lexer::new("#\\return");
        assert_eq!(Token::Character('\u{000D}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,9), lexer.pos());
    }

    #[test]
    fn test_character_space(){
        let mut lexer = Lexer::new("#\\space");
        assert_eq!(Token::Character('\u{0020}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,8), lexer.pos());
    }

    #[test]
    fn test_character_tab(){
        let mut lexer = Lexer::new("#\\tab");
        assert_eq!(Token::Character('\u{0009}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,6), lexer.pos());
    }

    #[test]
    fn test_character_vtab(){
        let mut lexer = Lexer::new("#\\vtab");
        assert_eq!(Token::Character('\u{000B}'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,7), lexer.pos());
    }

    #[test]
    fn test_empty_character_name(){
        let mut lexer = Lexer::new("#\\");
        assert_eq!(LexerError::BadCharName,lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }

    #[test]
    fn test_incomplete_character_name(){
        let mut lexer = Lexer::new("#\\retur");
        assert_eq!(LexerError::BadCharName,lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }

    #[test]
    fn test_bad_character_name(){
        let mut lexer = Lexer::new("#\\returd");
        assert_eq!(LexerError::BadCharName,lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }
    
    #[test]
    fn test_overlong_character_name(){
        let mut lexer = Lexer::new("#\\returnd");
        assert_eq!(LexerError::BadCharName,lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }
    
    #[test]
    fn test_ambiguous_character_literal(){
        let mut lexer = Lexer::new("#\\t"); //might be confused with start of char name 'tab'
        assert_eq!(Token::Character('t'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,4), lexer.pos());
    }

    #[test]
    fn test_ambiguous_character_literal_with_delim(){
        let mut lexer = Lexer::new("#\\t "); //might be confused with start of char name 'tab'
        assert_eq!(Token::Character('t'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,4), lexer.pos());
    }
    
    #[test]
    fn test_unambiguous_character_literal(){
        let mut lexer = Lexer::new("#\\q");
        assert_eq!(Token::Character('q'),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,4), lexer.pos());
    }

    #[test]
    fn test_hex_character(){
        let mut lexer = Lexer::new("#\\x20");
        assert_eq!(Token::Character(' '),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,6), lexer.pos());
    }

    #[test]
    fn test_hex_character_with_delim(){
        let mut lexer = Lexer::new("#\\x20 ");
        assert_eq!(Token::Character(' '),lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,6), lexer.pos());
    }

    #[test]
    fn test_bad_hex_character(){
        let mut lexer = Lexer::new("#\\x20t");
        assert_eq!(LexerError::BadCharHex,lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }

    #[test]
    fn test_invalid_hex_character(){
        let mut lexer = Lexer::new("#\\x20FFF");
        assert_eq!(LexerError::InvalidCharHex,lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,3), lexer.pos());
    }
}
