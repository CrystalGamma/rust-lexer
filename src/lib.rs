#![feature(macro_rules)]

use std::io::{Buffer, IoResult, IoError, EndOfFile};
use std::fmt::{Show, Formatter, FormatError, Arguments};

#[deriving(PartialEq,Clone)]
/// A token of Rust source code
pub struct Token {
	pub content: TokenContent,
	pub line: uint,
	/// starting column of the token
	pub start: uint,
	/// end column of the token
	pub end: uint
}
#[deriving(PartialEq,Clone,Eq)]
pub enum TokenContent {
	Identifier(String),
	Lifetime(String),
	Arrow, // =>
	Equals, // ==
	Scope, // ::
	UnEqual, // !=
	Char(char),
	Other(char)
}

impl Show for TokenContent { 
	fn fmt(&self, format: &mut Formatter) -> Result<(), FormatError> {
		match *self {
			Identifier(ref s) => format.pad(s.as_slice()),
			Lifetime(ref s) => format_args!(
				|args: &Arguments|{args.fmt(format)},
				"'{}", s),
			Arrow => format.pad("'=>'"),
			Equals => format.pad("'=='"),
			Scope => format.pad("'::'"),
			UnEqual => format.pad("'!='"),
			Char(c) => format_args!(
				|args: &Arguments|{args.fmt(format)},
				"'{}'", c),
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
						content: Other('='),
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
						content: Other(':'),
						line: self.line,
						start: col,
						end: self.column
					}))
				}
			},
			'!' => {
				let col = self.column;
				proceed!();
				if self.lookahead == '=' {
					proceed!();
					Some(Ok(Token {
						content: UnEqual,
						line: self.line,
						start: col,
						end: self.column
					}))
				} else {
					Some(Ok(Token {
						content: Other('!'),
						line: self.line,
						start: col,
						end: self.column
					}))
				}
			},
			'/' => {
				let col = self.column;
				proceed!();
				match self.lookahead {
					'/' => {
						proceed!();
						while self.lookahead != '\n' {
							proceed!();
						}
						self.next()
					},
					_ => Some(Ok(Token {
						content: Other('/'),
						line: self.line,
						start: col,
						end: self.column
					}))
				}
			}
			'\'' => {
				let col = self.column;
				proceed!();
				if self.lookahead.is_XID_start() || self.lookahead == '_' {
					let tok = match self.next() {
						Some(Ok(x)) => x,
						Some(Err(e)) => return Some(Err(e)),
						None => return Some(Err(IoError {
							kind: EndOfFile,
							desc: "End of file while reading Character literal",
							detail: None
						}))
					};
					match tok.content {
						Identifier(id) => if id.as_slice().char_len() > 1 || self.lookahead != '\'' {
							Some(Ok(Token {
								content: Lifetime(id),
								line: self.line,
								start: col,
								end: self.column
							}))
						} else {
							proceed!();
							Some(Ok(Token {
								content: Char(id.as_slice().char_at(0)),
								line: self.line,
								start: col,
								end: self.column
							}))
						},
						_ => fail!()
					}
				} else {
					unimplemented!();
				}
			}
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