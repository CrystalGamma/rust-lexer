#![feature(macro_rules)]

use std::io::{Buffer, IoResult};
use std::fmt::{Show, Formatter, FormatError, Arguments};

/*macro_rules! iotry(
	($e:expr) => (match $e {
		Ok(x) => x,
		Err(ref e) if e.kind != => return Some(Err(e))
	}));*/

#[allow(dead_code)]
/// A token of Rust source code
pub struct Token {
	content: TokenContent,
	line: uint,
	/// starting column of the token
	start: uint,
	/// end column of the token
	end: uint
}
pub enum TokenContent {
	Identifier(String),
	Other
}

impl Show for Token {
	fn fmt(&self, format: &mut Formatter) -> Result<(), FormatError> {
		match self.content {
			Identifier(ref id) => format_args!(
				|args: &Arguments|{args.fmt(format)},
				"{:u}:{:u}-{:u}: id {:s}",
				self.line,
				self.start,
				self.end,
				*id),
			Other => format_args!(
				|args: &Arguments|{args.fmt(format)},
				"{:u}:{:u}-{:u}: other",
				self.line,
				self.start,
				self.end)
		}
	}
}

pub struct Lexer<T> {
	read: T,
	lookahead: char
}

impl<T: Buffer> Lexer<T> {
	pub fn new(mut read: T) -> IoResult<Lexer<T>> {
		let la = try!(read.read_char());
		Ok(Lexer {read: read, lookahead: la})
	}
}

impl<T: Buffer> Iterator<IoResult<Token>> for Lexer<T> {
	fn next(&mut self) -> Option<IoResult<Token>> {
		
		match self.read.read_char() {
			Ok(c) => { self.lookahead = c; },
			Err(e) => return Some(Err(e))
		};
		Some(Ok(Token {content: Other, line:0, start: 0, end: 0}))
	}
}