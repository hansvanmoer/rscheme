///
/// The lexer
///

use crate::pos::Pos;
use crate::number::Number;
use crate::number::Real;

use unic_ucd_category::GeneralCategory;

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
    InvalidCharHex,

    ///
    /// Invalid character escape
    ///
    BadCharEscape,

    ///
    /// The supplied prefixed datum is neither a boolean nor a character nor a number prefix
    ///
    BadDatumPrefix,
    
    ///
    /// Bad number
    ///
    BadNumber
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
    Character(char),

    ///
    /// A token representing a literal string
    ///
    String(String),

    ///
    /// A token representing a symbol
    ///
    Symbol(String),

    ///
    /// A token representing a number
    ///
    Number(Number)
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

    fn is_leading_symbol_char(cc: & char) -> bool {
        let c :char  = *cc;
        if c.is_ascii_alphabetic() || c == '!' || c == '$' || c == '%' || c == '&' || c == '*' || c == '/' || c == ':' || c == '<' || c == '=' || c == '>' || c == '?' || c == '~' || c == '_' || c == '^' {
            true
        }else{
            match GeneralCategory::of(c) {
                GeneralCategory::UppercaseLetter | GeneralCategory::LowercaseLetter | GeneralCategory::TitlecaseLetter | GeneralCategory::ModifierLetter | GeneralCategory::OtherLetter | GeneralCategory::NonspacingMark | GeneralCategory::LetterNumber | GeneralCategory::OtherNumber | GeneralCategory::DashPunctuation | GeneralCategory::ConnectorPunctuation | GeneralCategory::OtherPunctuation | GeneralCategory::CurrencySymbol | GeneralCategory::MathSymbol | GeneralCategory::ModifierSymbol | GeneralCategory::OtherSymbol | GeneralCategory::PrivateUse => true,
                _ => false
            }
        }
    }

    fn is_trailing_symbol_char(c: & char) -> bool{
        if Lexer::is_leading_symbol_char(c) || c.is_ascii_digit() || c == &'.' || c == &'+' || c == &'-' || c == &'@' {
            true
        }else {
            match GeneralCategory::of(*c) {
                GeneralCategory::DecimalNumber | GeneralCategory::SpacingMark | GeneralCategory::EnclosingMark => true,
                _ => false
            }
        }
    }

    fn is_leading_number_char(cc: & char) -> bool{
        let c = *cc;
        c == '#'
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

    fn lex_hex_escape(& mut self) -> Result<char, LexerError> {
        let mut buf = String::new();
        loop {
            match self.peek() {
                Some(c) => {
                    if Lexer::is_hex_char(&c) {
                        buf.push(self.next_non_breaking().unwrap());
                    }else if Lexer::is_character_hex_delim_char(&c) {
                        break;
                    }else{
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
                    Ok(std::char::from_u32(val).unwrap())
                }else{
                    Err(LexerError::InvalidCharHex)
                }
            },
            Err(_) => {
                Err(LexerError::BadCharHex)
            }
        }
    }
    
    fn lex_character_hex(& mut self) -> Result<Token, LexerError> {
        let pos = self.pos();
        self.skip_non_breaking();
        match self.lex_hex_escape() {
            Ok(c) => {
                Ok(Token::Character(c))
            },
            Err(e) =>{
                self.reset_pos(pos);
                Err(e)
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

    fn lex_hash_datum(& mut self) -> Result<Token, LexerError> {
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
                    'i' | 'e' | 'b' | 'o' | 'd' | 'x' => {
                        self.lex_prefixed_number()
                    },
                    _ => Err(LexerError::BadDatumPrefix)
                }
            },
            None => Err(LexerError::BadEnd)
        }
    }

    fn lex_string(& mut self) -> Result<Token, LexerError> {
        let pos = self.pos();
        self.skip_non_breaking();
        let mut buf = String::new();
        loop{
            match self.next() {
                Some(c) => {
                    match c {
                        '\"' => {
                            return Ok(Token::String(buf));
                        },
                        '\\' => {
                            match self.next() {
                                Some(e) => {
                                    buf.push(match e {
                                        '\\' => '\\',
                                        'a' => '\u{7}',
                                        'b' => '\u{8}',
                                        'f' => '\u{c}',
                                        'n' => '\n',
                                        'r' => '\r',
                                        't' => '\t',
                                        'v' => '\u{B}',
                                        'x' => {
                                            match self.lex_hex_escape() {
                                                Ok(h) => {
                                                    h
                                                },
                                                Err(e) => {
                                                    self.reset_pos(pos);
                                                    return Err(e);
                                                }
                                            }
                                           
                                        }
                                        _ => {
                                            self.reset_pos(pos);
                                            return Err(LexerError::BadCharEscape);
                                        }
                                    });
                                },
                                None => {
                                    return Err(LexerError::BadEnd);
                                }
                            }
                        }
                        _ => {
                            buf.push(c);
                        }
                    }
                },
                None => {
                    self.reset_pos(pos);
                    return Err(LexerError::BadEnd);
                }
            }
        }
    }

    fn lex_symbol(& mut self) -> Result<Option<Token>, LexerError> {
        match self.peek() {
            Some(c) => {
                if Lexer::is_leading_symbol_char(c){
                     Ok(Some(self.lex_symbol_tested()?))
                }else{
                    Ok(None)
                }
            },
            None => {
                Ok(None)
            }
        }
    }
    
    fn lex_symbol_tested(& mut self) -> Result<Token, LexerError> {
        let pos = self.pos();
        let mut buf = String::new();
        loop{
            match self.peek() {
                Some(c) => {
                    if *c == '\\' {
                        self.skip_non_breaking();
                        match self.next() {
                            Some(x) => {
                                match x {
                                    'x' => {
                                        match self.lex_hex_escape() {
                                            Ok(c) => {
                                                buf.push(c);
                                            },
                                            Err(e) => {
                                                self.reset_pos(pos);
                                                return Err(e);
                                            }
                                        }
                                    },
                                    _ => {
                                        self.reset_pos(pos);
                                        return Err(LexerError::BadCharEscape);
                                    }
                                }
                            },
                            None => {
                                self.reset_pos(pos);
                                return Err(LexerError::BadEnd);
                            }
                        }
                    }else if Lexer::is_trailing_symbol_char(c) {
                        buf.push(self.next().unwrap());
                    }else{
                        break;
                    }
                },
                None => {
                    break;
                }
            }
        }
        Ok(Token::Symbol(buf))
    }


    fn lex_real_or_imag(& mut self, radix: &u32, exactness: &bool) -> Result<(bool, Real), LexerError> {
        return Ok((false, Real::new_unit()))
    }

    fn lex_imag_or_nothing(& mut self, radix: &u32, exactness: &bool) -> Result<Real, LexerError> {
        return Ok(Real::new_zero())
    }
    
    fn lex_complex(& mut self, radix: u32, exactness: bool) -> Result<Number, LexerError> {
        match self.lex_real_or_imag(&radix, &exactness) {
            Ok((true, imag)) => {
                Ok(Number::new(radix, exactness, Real::new_zero(), imag))
            },
            Ok((false, real)) => {
                Ok(Number::new(radix, exactness, real, self.lex_imag_or_nothing(&radix, &exactness)?))
            },
            _ => {
                Err(LexerError::BadNumber)
            }
        }
    }
    
    fn lex_prefixed_number(& mut self) -> Result<Token, LexerError> {
        let pos = Pos::new(self.line(), self.col() -1);
        let mut radix = 10;
        let mut radix_defined = false;
        let mut exactness = false;
        let mut exactness_defined = false;

        match self.next() {
            Some(c) => {
                match c {
                    'b' => {
                        radix = 2;
                        radix_defined = true;
                    },
                    'o' => {
                        radix = 8;
                        radix_defined = true;
                    },
                    'd' => {
                        radix = 10;
                        radix_defined = true;
                    },
                    'x' => {
                        radix = 16;
                        radix_defined = true;
                    },
                    'i' => {
                        exactness = false;
                        exactness_defined = true;
                    },
                    'e' => {
                        exactness = true;
                        exactness_defined = true;
                    },
                    _ => {
                        self.reset_pos(pos);
                        return Err(LexerError::BadNumber);
                    }
                }
            },
            None => {
                self.reset_pos(pos);
                return Err(LexerError::BadEnd);
            }
        }
        
        match self.peek() {
            Some(c) => {
                if *c == '#' {
                    self.skip_non_breaking();
                    match self.next() {
                        Some(b) => {
                            match b {
                                'b' => {
                                    if radix_defined {
                                        self.reset_pos(pos);
                                        return Err(LexerError::BadNumber);
                                    }else{
                                        radix = 2;
                                        radix_defined = true;
                                    }
                                },
                                'o' => {
                                    if radix_defined {
                                        self.reset_pos(pos);
                                        return Err(LexerError::BadNumber);
                                    }else{
                                        radix = 8;
                                        radix_defined = true;
                                    }
                                },
                                'd' => {
                                    if radix_defined {
                                        self.reset_pos(pos);
                                        return Err(LexerError::BadNumber);
                                    }else{
                                        radix = 10;
                                        radix_defined = true;
                                    }
                                },
                                'x' => {
                                    if radix_defined {
                                        self.reset_pos(pos);
                                        return Err(LexerError::BadNumber);
                                    }else{
                                        radix = 16;
                                        radix_defined = true;
                                    }
                                },
                                'i' => {
                                    if exactness_defined {
                                        self.reset_pos(pos);
                                        return Err(LexerError::BadNumber);
                                    }else{
                                        exactness = false;
                                        exactness_defined = true;
                                    }
                                },
                                'e' => {
                                    if exactness_defined {
                                        self.reset_pos(pos);
                                        return Err(LexerError::BadNumber);
                                    }else{
                                        exactness = true;
                                        exactness_defined = true;
                                    }
                                },
                                _ => {
                                    self.reset_pos(pos);
                                    return Err(LexerError::BadNumber);
                                }
                            }
                        },
                        None => {
                            self.reset_pos(pos);
                            return Err(LexerError::BadNumber);
                        }
                    }
                }
            },
            None => {
                self.reset_pos(pos);
                return Err(LexerError::BadEnd);
            }
        }        

        match self.lex_complex(radix, exactness) {
            Ok(number) => {
                Ok(Token::Number(number))
            },
            Err(e) => {
                self.reset_pos(pos);
                Err(e)
            }
        }
    }
    
    pub fn lex(& mut self) -> Result<Option<Token>, LexerError> {
        match self.peek() {
            Some(c) => {
                match c {
                    '#' => Ok(Some(self.lex_hash_datum()?)),
                    '\"' => Ok(Some(self.lex_string()?)),
                    _ => {
                        match self.lex_symbol() {
                            Ok(result) => {
                                match result {
                                    Some(token) => {
                                        Ok(Some(token))
                                    },
                                    None => {
                                        Err(LexerError::BadChar)
                                    }
                                }
                            },
                            Err(e) => {
                                Err(e)
                            }
                        }
                    }
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

    use crate::number::Number;
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
    fn test_bad_prefixed_datum() {
        let mut lexer = Lexer::new("#r");
        assert_eq!(LexerError::BadDatumPrefix, lexer.lex().err().unwrap());
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

    #[test]
    fn text_simple_string(){
        let mut lexer = Lexer::new("\"test\"");
        assert_eq!(Token::String(String::from("test")), lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,7), lexer.pos());
    }

    #[test]
    fn test_unterm_string(){
        let mut lexer = Lexer::new("\"test");
        assert_eq!(LexerError::BadEnd, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,1), lexer.pos());
    }

    #[test]
    fn test_string_escapes(){
        let mut lexer = Lexer::new("\"\\\\ \\a \\b \\f \\n \\r \\t \\v\"");
        assert_eq!(Token::String(String::from("\\ \u{7} \u{8} \u{c} \n \r \t \u{B}")), lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,26), lexer.pos());
    }

    #[test]
    fn test_bad_string_escape(){
        let mut lexer = Lexer::new("\"\\d\"");
        assert_eq!(LexerError::BadCharEscape, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,1), lexer.pos());
    }

    #[test]
    fn test_string_hex_escapes(){
    let mut lexer = Lexer::new("\"\\x7 \\x8 \\xc \\xB\"");
        assert_eq!(Token::String(String::from("\u{7} \u{8} \u{c} \u{B}")), lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,18), lexer.pos());     
    }

    #[test]
    fn test_bad_string_hex_escape(){
        let mut lexer = Lexer::new("\"\\xJ\"");
        assert_eq!(LexerError::BadCharHex, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,1), lexer.pos());
    }

    #[test]
    fn test_symbol(){
        let mut lexer = Lexer::new("abc3");
        assert_eq!(Token::Symbol(String::from("abc3")), lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,5), lexer.pos());
    }

    #[test]
    fn test_symbol_with_delim(){
        let mut lexer = Lexer::new("abc3 ");
        assert_eq!(Token::Symbol(String::from("abc3")), lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,5), lexer.pos());
    }

    #[test]
    fn test_symbol_with_hex_escape(){
        let mut lexer = Lexer::new("abc3\\x20");
        assert_eq!(Token::Symbol(String::from("abc3 ")), lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,9), lexer.pos());
    }

    #[test]
    fn test_symbol_with_leading_hex_escape(){
        let mut lexer = Lexer::new("\\x20\\x26");
        assert_eq!(Token::Symbol(String::from(" &")), lexer.lex().unwrap().unwrap());
        assert_eq!(Pos::new(1,9), lexer.pos());
    }

    #[test]
    fn test_double_exactness_number(){
        let mut lexer = Lexer::new("#e#e");
        assert_eq!(LexerError::BadNumber, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,1), lexer.pos());

        let mut lexer = Lexer::new("#e#i");
        assert_eq!(LexerError::BadNumber, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,1), lexer.pos());

        let mut lexer = Lexer::new("#i#e");
        assert_eq!(LexerError::BadNumber, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,1), lexer.pos());

        let mut lexer = Lexer::new("#i#i");
        assert_eq!(LexerError::BadNumber, lexer.lex().err().unwrap());
        assert_eq!(Pos::new(1,1), lexer.pos());
    }
    
    #[test]
    fn test_prefixed_exact_base2_number(){
        //let mut lexer = Lexer::new("#b");
        //assert_eq!(Token::Number(Number::new(2,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#e#b");
        assert_eq!(Token::Number(Number::new_unit(2,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#i#b");
        assert_eq!(Token::Number(Number::new_unit(2,false)), lexer.lex().unwrap().unwrap());
    }

    #[test]
    fn test_prefixed_exact_base8_number(){
        //let mut lexer = Lexer::new("#o");
        //assert_eq!(Token::Number(Number::new(8,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#e#o");
        assert_eq!(Token::Number(Number::new_unit(8,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#i#o");
        assert_eq!(Token::Number(Number::new_unit(8,false)), lexer.lex().unwrap().unwrap());
    }

    #[test]
    fn test_prefixed_exact_base10_number(){
        //let mut lexer = Lexer::new("#o");
        //assert_eq!(Token::Number(Number::new(8,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#e#d");
        assert_eq!(Token::Number(Number::new_unit(10,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#i#d");
        assert_eq!(Token::Number(Number::new_unit(10,false)), lexer.lex().unwrap().unwrap());
    }

    #[test]
    fn test_prefixed_exact_base16_number(){
        //let mut lexer = Lexer::new("#o");
        //assert_eq!(Token::Number(Number::new(8,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#e#x");
        assert_eq!(Token::Number(Number::new_unit(16,true)), lexer.lex().unwrap().unwrap());

        let mut lexer = Lexer::new("#i#x");
        assert_eq!(Token::Number(Number::new_unit(16,false)), lexer.lex().unwrap().unwrap());
    }
}
