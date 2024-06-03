//! The Span module defines source positions, and ranges of source positions,
//! in which nodes can be defined. It is used to link nodes back to their
//! origins in the source.

use std::fmt;

/// A Position represents an arbitrary source position. It includes the
/// filename, line number, and column number.
#[derive(Clone, Debug, PartialEq)]
pub struct Position {
    filename: String,
    line: usize,
    column: usize,
}

impl Position {
    /// Create a new Position.
    ///
    /// # Arguments
    /// * `filename` The name of the file.
    /// * `line` The line number, where the first line of the file is at zero.
    /// * `column` The column number, where the first column of a line is at zero.
    ///
    /// # Return
    /// A new Position.
    pub fn new<Filename>(filename: Filename, line: usize, column: usize) -> Position
    where
        Filename: ToString,
    {
        Position {
            filename: filename.to_string(),
            line: line,
            column: column,
        }
    }

    /// # Return
    /// The name of the file in which the Position exists.
    pub fn filename(&self) -> &String {
        &self.filename
    }

    /// # Return
    /// The line number of the Position.
    pub fn line(&self) -> usize {
        self.line
    }

    /// # Return
    /// The column number of the Position.
    pub fn column(&self) -> usize {
        self.column
    }
}

impl fmt::Display for Position {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        if self.filename.len() > 0 {
            write!(formatter, "{}:{}:{}", self.filename, self.line, self.column)
        } else {
            write!(formatter, "{}:{}", self.line, self.column)
        }
    }
}

/// A Span represents an arbitrary source range. It includes the beginning and
/// ending Positions.
#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub begin: Position,
    pub end: Position,
}

impl Span {
    /// Create a new Span. The beginning and ending Positions will also be
    /// created. The beginning and ending Positions are guaranteed to have the
    /// same filename.
    ///
    /// # Arguments
    /// * `filename` The name of the file. This name will be used for the
    ///   beginning and ending Positions.
    /// * `begin_line` The line number for the beginning of the Span, where
    ///   the first line of the file is at zero.
    /// * `begin_column` The column number for the beginning of the Span,
    ///   where the first column of a line is at zero.
    /// * `end_line` The line number for the ending of the Span, where
    ///   the first line of the file is at zero.
    /// * `end_column` The column number for the ending of the Span,
    ///   where the first column of a line is at zero.
    ///
    /// # Return
    /// A new Span, with new beginning and ending Positions.
    pub fn new<Filename>(
        filename: Filename,
        begin_line: usize,
        begin_column: usize,
        end_line: usize,
        end_column: usize,
    ) -> Span
    where
        Filename: Clone + ToString,
    {
        Span {
            begin: Position::new(filename.clone(), begin_line, begin_column),
            end: Position::new(filename.clone(), end_line, end_column),
        }
    }

    /// Create a new Span from beginning and ending positions. The positions
    /// are not guaranteed to have the same filename.
    ///
    /// # Arguments
    /// * `begin` The beginning position.
    /// * `end` The ending position.
    ///
    /// # Return
    /// A new Span, with the beginning and ending Positions.
    pub fn from_begin_end<Pos>(begin: Pos, end: Pos) -> Span
    where
        Pos: Into<Position>,
    {
        Span {
            begin: begin.into(),
            end: end.into(),
        }
    }
}

pub trait Spanned {
    fn set_span_begin(&mut self, begin: Position) {
        self.span_mut().begin = begin;
    }
    fn set_span_end(&mut self, end: Position) {
        self.span_mut().end = end;
    }
    fn span(&self) -> &Span;
    fn span_mut(&mut self) -> &mut Span;
}
