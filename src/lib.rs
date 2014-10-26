#![feature(macro_rules)]

use std::io::{Buffer, IoResult, EndOfFile};
use std::fmt::{Show, Formatter, FormatError, Arguments};

#[deriving(PartialEq)]
/// A token of Rust source code
pub struct Token {
	content: TokenContent,
	line: uint,
	/// starting column of the token
	start: uint,
	/// end column of the token
	end: uint
}
#[deriving(PartialEq)]
pub enum TokenContent {
	Identifier(String),
	DelimOpen(Delimiter),
	DelimClose(Delimiter),
	Assign, // =
	Arrow, // =>
	Equals, // ==
	Colon, // :
	Scope, // ::
	Other(char)
}
#[deriving(PartialEq)]
pub enum Delimiter {
	Brace,
	Parenthesis,
	Bracket
}

impl Show for TokenContent { 
	fn fmt(&self, format: &mut Formatter) -> Result<(), FormatError> {
		match *self {
			Identifier(ref s) => format.pad(s.as_slice()),
			DelimOpen(typ) => format.pad(match typ {
				Brace => "'{'",
				Parenthesis => "'('",
				Bracket => "'['",
			}),
			DelimClose(typ) => format.pad(match typ {
				Brace => "'}'",
				Parenthesis => "')'",
				Bracket => "']'",
			}),
			Assign => format.pad("'='"),
			Arrow => format.pad("'=>'"),
			Equals => format.pad("'=='"),
			Colon => format.pad("':'"),
			Scope => format.pad("'::'"),
			Other(c) => format_args!(
				|args: &Arguments|{args.fmt(format)},
				"other: '{}'", c)
		}
	}
}

impl Show for Token {
	fn fmt(&self, format: &mut Formatter) -> Result<(), FormatError> {
		format_args!(
			|args: &Arguments|{args.fmt(format)},
			"{:u}:{:u}-{:u}: {}",
			self.line,
			self.start,
			self.end,
			self.content)
	}
}

pub struct Lexer<T> {
	read: T,
	lookahead: char,
	line: uint,
	column: uint
}

impl<T: Buffer> Lexer<T> {
	pub fn new(mut read: T) -> IoResult<Lexer<T>> {
		let la = try!(read.read_char());
		Ok(Lexer {
			read: read,
			lookahead: la,
			line: 1,
			column: 1
		})
	}
	
	fn skip_whitespace(&mut self) -> IoResult<bool> {
		while self.lookahead.is_whitespace() {
			if self.lookahead == '\r' {
				match self.read.read_char() {
					Ok(c) => { self.lookahead = c; },
					Err(ref e) if e.kind == EndOfFile => {
						self.lookahead = '\0';
						return Ok(true);
					},
					Err(e) => return Err(e)
				};
				if self.lookahead != '\n' { // handle the Windows-style line endings as one line ending
					self.line += 1;
					self.column = if cfg!(lines_start_at_zero) { 0 } else { 1 };
				}
			}
			if self.lookahead == '\n' {
				self.line += 1;
				self.column = if cfg!(lines_start_at_zero) { 0 } else { 1 };
			} else {
				self.column += 1;
			}
			match self.read.read_char() {
				Ok(c) => { self.lookahead = c; },
				Err(ref e) if e.kind == EndOfFile => {
					self.lookahead = '\0';
					return Ok(true);
				},
				Err(e) => return Err(e)
			};
		}
		Ok(false)
	}
}

impl<T: Buffer> Iterator<IoResult<Token>> for Lexer<T> {
	fn next(&mut self) -> Option<IoResult<Token>> {
		macro_rules! proceed(
			()=>({
				self.column += 1;
				match self.read.read_char() {
					Ok(c) => { self.lookahead = c; },
					Err(ref e) if e.kind == EndOfFile => { self.lookahead = '\0'; },
					Err(e) => return Some(Err(e))
				}
			}));
		if self.lookahead == '\0' {
			return None;
		}
		match self.skip_whitespace() {
			Ok(false) => {},
			Ok(true) => return None,
			Err(e) => return Some(Err(e))
		}
		match self.lookahead {
			c @ '{' | c @  '(' | c @ '[' | c @ ']' | c @ ')' | c @ '}' => {
				let col = self.column;
				proceed!();
				Some(Ok(Token {
					content: match c {
						'{' => DelimOpen(Brace),
						'}' => DelimClose(Brace),
						'(' => DelimOpen(Parenthesis),
						')' => DelimClose(Parenthesis),
						'[' => DelimOpen(Bracket),
						']' => DelimClose(Bracket),
						_ => unreachable!()
					},
					line: self.line,
					start: col,
					end: self.column
				}))
			},
			'=' => {
				let col = self.column;
				proceed!();
				if self.lookahead == '>' {
					proceed!();
					Some(Ok(Token {
						content: Arrow,
						line: self.line,
						start: col,
						end: self.column
					}))
				} else if self.lookahead == '=' {
					proceed!();
					Some(Ok(Token {
						content: Equals,
						line: self.line,
						start: col,
						end: self.column
					}))
				} else {
					Some(Ok(Token {
						content: Assign,
						line: self.line,
						start: col,
						end: self.column
					}))
				}
			},
			':' => {
				let col = self.column;
				proceed!();
				if self.lookahead == ':' {
					proceed!();
					Some(Ok(Token {
						content: Scope,
						line: self.line,
						start: col,
						end: self.column
					}))
				} else {
					Some(Ok(Token {
						content: Colon,
						line: self.line,
						start: col,
						end: self.column
					}))
				}
			},
			_ if self.lookahead.is_XID_start() || self.lookahead == '_' => {
				let start = self.column;
				let mut id: Vec<char> = Vec::with_capacity(16);
				id.push(self.lookahead);
				'a: loop {
					match self.read.read_char() {
						Ok(c) => {
							self.column += 1;
							if c.is_XID_continue() {
								id.push(c);
							} else {
								self.lookahead = c;
								break 'a;
							}
						},
						Err(ref e) if e.kind == EndOfFile => {
							self.lookahead = '\0';
							break 'a;
						},
						Err(e) => return Some(Err(e))
					}
				}
				let str_ = String::from_chars(id.as_slice());
				Some(Ok(Token {
					content: Identifier(str_),
					line: self.line,
					start: start,
					end: self.column
				}))
			},
			c => {
				match self.read.read_char() {
					Ok(c) => { self.lookahead = c; },
					Err(ref e) if e.kind == EndOfFile => { self.lookahead = '\0'; },
					Err(e) => return Some(Err(e))
				};
				Some(Ok(Token {
					content: Other(c),
					line: self.line,
					start: self.column,
					end: self.column + 1
				}))
			}
		}
	}
}

#[test]
fn lex_empty() {
	let line_start = if cfg!(lines_start_at_zero) { 0 } else { 1 };
	let lex = match Lexer::new(
		std::io::BufferedReader::new(
			std::io::MemReader::new(std::vec::as_vec(b"fn main() {\n}").clone()))) {
		Ok(x) => x,
		Err(e) => fail!("{}", e)
	};
	let expected = vec!(
		Token {content: Identifier("fn".to_string()), line: line_start, start: 1, end: 3},
		Token {content: Identifier("main".to_string()),line: line_start, start: 4, end: 8},
		Token {content: DelimOpen(Parenthesis), line: line_start, start: 8, end: 9},
		Token {content: DelimClose(Parenthesis), line: line_start, start: 9, end: 10},
		Token {content: DelimOpen(Brace), line: line_start, start: 11, end: 12},
		Token {content: DelimClose(Brace), line: line_start + 1, start: 1, end: 2});
	for (a, b) in lex.zip(expected.iter()) {
		let act = match a {
			Ok(x) => x,
			Err(e) => fail!("{}", e)
		};
		println!("{} {}", act, b);
		assert_eq!(act, *b);
	}
}