use std::fmt;

pub struct Output {
    pub lines: Vec<Line>,
}

pub struct Line {
    pub left: String,
    pub right: String,
}

impl Output {
    pub fn add_line(&mut self, left: impl Into<String>, right: impl Into<String>) {
        self.lines.push(Line {
            left: left.into(),
            right: right.into()
        });
    }
}

impl fmt::Display for Output {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        let mut left_len = 0;
        for line in self.lines.iter() {
            left_len = std::cmp::max(left_len, line.left.len());
        }
        for (lineno, line) in self.lines.iter().enumerate() {
            write!(f, "{}", line.left)?;
            for _ in line.left.len()..left_len {
                write!(f, " ")?;
            }
            write!(f, "  │  ")?;
            write!(f, "{}", line.right)?;
            if lineno + 1 < self.lines.len() {
                write!(f, "\n")?;
            }
        }
        Ok(())
    }
}
