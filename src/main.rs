pub mod parser;
pub mod jit;

use std::io::{self, Read};

use crate::parser::get_functions;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    EOF,
    ENTRY,
    DEF,
    EXTERN,

    IF,
    ELSE,
    WHILE,
    FOR,

    BREAK,
    IFBREAK,

    IDENTIFIER(String),
    //COLON,

    INT(i64),
    //FLOAT(f64),
    STRING(String),

    PLUS,
    MINUS,
    TIMES,
    DIVIDE,
    MODULUS,
    EQUALS,
    GREATER,
    LESS,
    NOT,

    POW,

    BITAND,
    BITOR,
    BITXOR,
    BITNOT,
    BITLSHIFT,
    BITRSHIFT,

    LPAREN,
    RPAREN,

    LCURLY,
    RCURLY,
}

impl Token {
    fn to_string(self: &Token) -> String {
        match self {
            Token::INT(int) => return int.to_string(),
            Token::STRING(str) => {
                let mut data = String::from(STRING_CHAR);
                data.push_str(str);
                data.push(STRING_CHAR);
                return data;
            }
            _ => (),
        };

        match self {
            Token::EOF => "EOF",
            Token::ENTRY => ENTRY_TEXT,
            Token::DEF => DEF_TEXT,
            Token::EXTERN => EXTERN_TEXT,
            Token::IF => IF_TEXT,
            Token::ELSE => ELSE_TEXT,
            Token::WHILE => WHILE_TEXT,
            Token::FOR => FOR_TEXT,
            Token::IDENTIFIER(str) => str,
            Token::POW => "^",
            Token::PLUS => "+",
            Token::MINUS => "-",
            Token::TIMES => "*",
            Token::DIVIDE => "/",
            Token::EQUALS => "=",
            Token::LPAREN => "(",
            Token::RPAREN => ")",
            Token::LCURLY => "{",
            Token::RCURLY => "}",
            Token::GREATER => ">",
            Token::LESS => "<",
            Token::NOT => "!",
            Token::MODULUS => "%",
            Token::BREAK => BREAK_TEXT,
            Token::BITAND => " and ",
            Token::BITOR => " or ",
            Token::BITXOR => " xor ",
            Token::BITNOT => " not ",
            Token::BITLSHIFT => " >> ",
            Token::BITRSHIFT => " << ",
            _ => "ERR"
        }
        .to_string()
    }
}

const ENTRY_TEXT: &str = "seed";
const DEF_TEXT: &str = "farm";
const EXTERN_TEXT: &str = "wild";
const IF_TEXT: &str = "if";
const ELSE_TEXT: &str = "else";
const WHILE_TEXT: &str = "while";
const FOR_TEXT: &str = "for";
const BREAK_TEXT: &str = "br";
const IFBREAK_TEXT: &str = "ifbr";
const STRING_CHAR: char = '\"';

const BITAND_TEXT: &str = "and";
const BITOR_TEXT: &str = "or";
const BITXOR_TEXT: &str = "xor";
const BITNOT_TEXT: &str = "not";
const BITLSHIFT_TEXT: &str = "left";
const BITRSHIFT_TEXT: &str = "right";

const SINGLE_LINE_COMMENT: char = '#';
const MULTI_LINE_COMMENT: char = '*';

fn next_char(indec: &mut std::str::CharIndices) -> Option<char> {
    if let Some((_, c)) = indec.next() {
        Some(c)
    } else {
        None
    }
}

fn get_tokens(buffer: &String) -> Vec<Token> {
    if buffer.len() <= 0 {
        return vec![];
    };

    let mut list: Vec<Token> = Vec::new();
    let mut indec = buffer.char_indices();

    let mut last_char: Option<char> = next_char(&mut indec);

    let mut comment_index = 0;
    let mut is_single_comment = false;

    loop {
        if let Some(currrent_char) = last_char {
            if is_single_comment {
                match currrent_char {
                    '\n' | SINGLE_LINE_COMMENT => {
                        is_single_comment = false;
                    }
                    _ => ()
                }
                last_char = next_char(&mut indec);
                continue;
            }

            if comment_index > 0 {
                last_char = next_char(&mut indec);
                match currrent_char {
                    SINGLE_LINE_COMMENT => {
                        if last_char == Some(MULTI_LINE_COMMENT) {
                            comment_index += 1;
                        }
                    }

                    MULTI_LINE_COMMENT => {
                        if last_char == Some(SINGLE_LINE_COMMENT) {
                            comment_index -= 1;
                        }
                    }

                    _ => ()
                }
                continue;   
            }

            if currrent_char == SINGLE_LINE_COMMENT {
                last_char = next_char(&mut indec);
                match last_char {
                    Some(MULTI_LINE_COMMENT) => {
                        comment_index = 1;
                    }
                    _ => {
                        is_single_comment = true;
                    }
                }
                continue;
            }
            
            if currrent_char.is_whitespace() {
                last_char = next_char(&mut indec);
                continue;
            }

            if currrent_char.is_alphabetic() {
                let mut identifier_str = String::new();
                identifier_str.push(currrent_char);
                loop {
                    last_char = next_char(&mut indec);
                    if let Some(c) = last_char {
                        if c.is_alphanumeric() {
                            identifier_str.push(c);
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }

                match &identifier_str[..] {
                    DEF_TEXT => {
                        list.push(Token::DEF);
                    }
                    EXTERN_TEXT => {
                        list.push(Token::EXTERN);
                    }
                    ENTRY_TEXT => {
                        list.push(Token::ENTRY);
                    }
                    IF_TEXT => {
                        list.push(Token::IF);
                    }
                    ELSE_TEXT => {
                        list.push(Token::ELSE);
                    }
                    WHILE_TEXT => {
                        list.push(Token::WHILE);
                    }
                    FOR_TEXT => {
                        list.push(Token::FOR);
                    }
                    BREAK_TEXT => {
                        list.push(Token::BREAK);
                    }
                    IFBREAK_TEXT => {
                        list.push(Token::IFBREAK);
                    }
                    BITAND_TEXT => {
                        list.push(Token::BITAND);
                    }
                    
                    BITLSHIFT_TEXT => {
                        list.push(Token::BITLSHIFT);
                    }
                    BITRSHIFT_TEXT => {
                        list.push(Token::BITRSHIFT);
                    }
                    
                    BITNOT_TEXT => {
                        list.push(Token::BITNOT);
                    }
                    BITOR_TEXT => {
                        list.push(Token::BITOR);
                    }
                    BITXOR_TEXT => {
                        list.push(Token::BITXOR);
                    }
                    _ => {
                        list.push(Token::IDENTIFIER(identifier_str));
                    }
                }
                continue;
            } else if currrent_char.is_numeric() {
                let mut identifier_str = String::new();
                identifier_str.push(currrent_char);
                //let mut is_float = false;
                loop {
                    last_char = next_char(&mut indec);
                    if let Some(c) = last_char {
                        // let is_dot = c == '.';
                        //if is_dot {
                        //    is_float = true;
                        //}
                        if c.is_numeric() {
                            //|| is_dot {
                            identifier_str.push(c);
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
                list.push(Token::INT(identifier_str.parse::<i64>().unwrap()));
                continue;
            } else if currrent_char == STRING_CHAR {
                // be aware that no closing char will result in all text being the same string
                let mut identifier_str = String::new();
                loop {
                    last_char = next_char(&mut indec);
                    if let Some(c) = last_char {
                        if c == STRING_CHAR {
                            break;
                        }
                        identifier_str.push(c);
                    } else {
                        break;
                    }
                }
                list.push(Token::STRING(identifier_str));
            } else {
                let symbol: Option<Token> = match currrent_char {
                    '+' => Some(Token::PLUS),
                    '-' => Some(Token::MINUS),
                    '*' => Some(Token::TIMES),
                    '/' => Some(Token::DIVIDE),
                    '(' => Some(Token::LPAREN),
                    ')' => Some(Token::RPAREN),
                    '^' => Some(Token::POW),
                    '{' => Some(Token::LCURLY),
                    '}' => Some(Token::RCURLY),
                    '=' => Some(Token::EQUALS),
                    '!' => Some(Token::NOT),
                    '>' => Some(Token::GREATER),
                    '<' => Some(Token::LESS),
                    '%' => Some(Token::MODULUS),
                    _ => None,
                };
                if let Some(token) = symbol {
                    list.push(token);
                }
            }

            last_char = next_char(&mut indec);
        } else {
            break;
        }
    }

    list.push(Token::EOF);
    list
}

fn main() -> io::Result<()> {
    let mut buffer : Vec<u8> = Vec::new() ;
    let mut stdin = io::stdin();
    stdin.read_to_end(&mut buffer)?;
    let str = std::str::from_utf8(&buffer).unwrap().to_string();
   // let tokens = get_tokens(&str );
   // println!("TOKENS: {}", tokens.len());
   // for token in tokens {
   //     println!("{:?}", token)
   // }
   // println!("DONE!");

    get_functions(get_tokens(&str));
    Ok(())
}
