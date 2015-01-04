/*
    rust-lexer - A lexical analyzer for the Rust programming language
    Copyright (C) 2014  Jona Stubbe

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/
#![feature(macro_rules)]
#![feature(globs)]

use std::io::{Buffer, IoResult, IoError, EndOfFile};
use std::fmt::{Show, Formatter, Error};

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
	StringLiteral(String),
	Arrow, // =>
	Equals, // ==
	Scope, // ::
	UnEqual, // !=
	Char(char),
	Other(char)
}

impl Show for TokenContent { 
	fn fmt(&self, format: &mut Formatter) -> Result<(), Error> {
		use TokenContent::*;
		match *self {
			Identifier(ref s) => format.pad(s.as_slice()),
			Lifetime(ref s) => format_args!("'{}", s).fmt(format),
			StringLiteral(ref s) => format_args!("\"{}\"", s).fmt(format),
			Arrow => format.pad("'=>'"),
			Equals => format.pad("'=='"),
			Scope => format.pad("'::'"),
			UnEqual => format.pad("'!='"),
			Char(c) => format_args!("'{}'", c).fmt(format),
			Other(c) => format_args!("other: '{}'", c).fmt(format)
		}
	}
}

impl Show for Token {
	fn fmt(&self, format: &mut Formatter) -> Result<(), Error> {
		format_args!("{}:{}-{}: {}",
			self.line,
			self.start,
			self.end,
			self.content).fmt(format)
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
	fn parsechar(&mut self) -> IoResult<char> {
		match self.lookahead {
			'\\' => {
				self.lookahead = try!(self.read.read_char());
				let c = match self.lookahead {
					'u' => unimplemented!(),
					'\\' => '\\',
					'\'' => '\'',
					'"' => '"',
					'n' => '\n',
					't' => '\t',
					_ => return Err(IoError {
						kind: std::io::OtherIoError,
						desc: "unknown escape sequence starting with '{}'",
						detail: None
					})
				};
				self.lookahead = match self.read.read_char() {
					Ok(c) => c,
					Err(ref e) if e.kind == EndOfFile => '\0',
					Err(e) => return Err(e)
				};
				Ok(c)
			},
			c @ _ => {
				self.lookahead = try!(self.read.read_char());
				Ok(c)
			}
		}
	}
}

impl<T: Buffer> Iterator<IoResult<Token>> for Lexer<T> {
	fn next(&mut self) -> Option<IoResult<Token>> {
		use TokenContent::*;
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
				if self.lookahead.is_xid_start() || self.lookahead == '_' {
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
						Identifier(id) => if id.as_slice().chars().count() > 1
							|| self.lookahead != '\'' { //TODO: better solution for chars().count() > 1
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
						_ => panic!()
					}
				} else {
					let col = self.column;
					let line = self.line;
					let c = match self.parsechar() {
						Ok(c) => c,
						Err(e) => return Some(Err(e))
					};
					match self.lookahead {
						'\0' => Some(Err(IoError {
							kind: EndOfFile,
							desc: "End of file while reading character literal",
							detail: None
						})),
						'\'' => {
							let end = self.column;
							proceed!();
							Some(Ok(Token {
								content: Char(c),
								line: line,
								start: col,
								end: end
							}))
						},
						_ => Some(Err(IoError {
							kind: std::io::OtherIoError,
							desc: "unclosed character literal",
							detail: None
						}))
					}
				}
			},
			'"' => {
				let start_line = self.line;
				let col = self.column;
				proceed!();
				let mut text: Vec<char> = Vec::new();
				while self.lookahead != '"' && self.lookahead != '\0' {
					text.push(match self.parsechar() {
						Ok(c) => c,
						Err(e) => return Some(Err(e))
					});
				}
				if self.lookahead == '\0' {
					return Some(Err(IoError {
						kind: EndOfFile,
						desc: "End of file while reading string literal",
						detail: None
					}));
				}
				proceed!();
				Some(Ok(Token {
					content: StringLiteral(text.into_iter().collect()),
					line: start_line,
					start: col,
					end: self.column
				}))
			},
			_ if self.lookahead.is_xid_start() || self.lookahead == '_' => {
				let start = self.column;
				let mut id: Vec<char> = Vec::with_capacity(16);
				id.push(self.lookahead);
				'a: loop {
					match self.read.read_char() {
						Ok(c) => {
							self.column += 1;
							if c.is_xid_continue() {
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
				let str_ = id.into_iter().collect();
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
		Err(e) => panic!("{}", e)
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
			Err(e) => panic!("{}", e)
		};
		println!("{} {}", act, b);
		assert_eq!(act, *b);
	}
}