use super::direction::*;
use super::editor_error::*;
use super::pos::*;
use std::fs::*;
use std::io::Read;

use ncurses as nc;
pub struct Buffer {
    data: Vec<Vec<char>>,
    cursorpos: Pos,
    beggining_line: u32,
    lines: u32,
    cols: u32,
    assoc_filename: Option<String>,
}

impl Buffer {
    pub fn mk_file_buf(filePath: &str) -> Result<Buffer, EditorError> {
        // unimplemented: make it lazy
        let f = File::open(filePath);
        let zeropos = Pos { x: 0, y: 0 };
        return match f {
            Ok(mut f) => {
                let mut dat = String::new();
                f.read_to_string(&mut dat)
                    .expect("could not read file as string"); // TODO: can elim copy using readline?
                let mut data: Vec<Vec<char>> = Vec::new();
                for line in dat.split("\n") {
                    data.push(line.to_string().chars().collect());
                }
                let lines: u32 = nc::LINES() as u32 - 1;
                let cols: u32 = nc::COLS() as u32 - 1;
                let beggining_line = zeropos.y;
                Ok(Buffer {
                    data,
                    cursorpos: zeropos,
                    beggining_line,
                    lines,
                    cols,
                    assoc_filename: Some(filePath.to_string()),
                })
            }
            Err(_e) => Err(EditorError {
                msg: format!("could not find file ${}", filePath),
            }),
        };
    }

    pub fn mk_empty_buf(lines:u32, cols:u32) -> Buffer {
        let mut data: Vec<Vec<char>> = Vec::new();
        data.push(Vec::new()); // push first line
        let cursorpos = Pos { x: 0, y: 0 };
        let beggining_line = cursorpos.y;
        return Buffer {
            data,
            cursorpos,
            beggining_line,
            lines,
            cols,
            assoc_filename: None,
        };
    }
    pub fn insert(&mut self, c: char) {
        let x = self.cursorpos.x as usize;
        let y = self.cursorpos.y as usize;
        self.data[y].insert(x, c);
        self.cursorpos.x = self.cursorpos.x + 1;
    }

    pub fn redraw_all(&self) {
        nc::clear();
        let end_of_draw: u32 =
            std::cmp::max(self.lines, self.data.len() as u32 - self.beggining_line);
        for line_idx in self.beggining_line..end_of_draw {
            let line: &Vec<char> = &self.data[(line_idx + self.beggining_line) as usize];
            let sline: String = line.into_iter().collect();
            nc::addstr(sline.as_str());
            nc::nl();
        }
    }

    pub fn redraw_pos(&mut self, pos: Pos) {
        unimplemented!();
    }

    pub fn move_cursor(&mut self, direction: Direction, offset: i32) {
        let x = self.cursorpos.x;
        let y = self.cursorpos.y;
        match direction {
            Direction::Horizontal => {
                if offset >= 0 {
                    let len = if self.data[y as usize].len() == 0 {
                        0
                    } else {
                        self.data[y as usize].len() as u32 - 1
                    };
                    self.cursorpos.x = std::cmp::max(x + offset as u32, len);
                } else {
                    let len = if self.data[x as usize].len() == 0 {
                        0
                    } else {
                        self.data[x as usize].len() as u32 - 1
                    };
                    self.cursorpos.x = std::cmp::min(x + offset as u32, len);
                }
            }
            Direction::Vertical => {
                if offset >= 0 {
                    let len = if self.data[y as usize].len() == 0 {
                        0
                    } else {
                        self.data[y as usize].len() as u32 - 1
                    };
                    self.cursorpos.y =
                        std::cmp::max(y + offset as u32, len);
                } else {
                    self.cursorpos.y = std::cmp::min(y + offset as u32, 0);
                }
                self.cursorpos.x = 
                    std::cmp::max(x, self.get_row_text_length(self.cursorpos.y as u32) as u32);
            }
        }
    }

    pub fn get_size(&self) -> Pos {
        return Pos {
            x: self.lines,
            y: self.cols,
        };
    }

    pub fn save_file(&self) {
        // TODO efficency, concurrency
        let mut content: String = String::new();
        for line in &self.data {
            content.push_str(line.into_iter().collect::<String>().as_str());
        }
    }

    fn get_row_text_length(&self, y:u32) -> usize {
        if y >= self.data.len() as u32 {
            0
        } else {
            self.data[y as usize].len() as usize
        }
    }

    pub fn add_newline(&mut self) {
        self.data.insert(self.cursorpos.y as usize, Vec::new());
        self.move_cursor(Direction::Vertical, 1);
    }
}

#[test]
fn test_newbuf() {
    let _b = Buffer::mk_empty_buf(10,10);
}

#[test]
fn test_insert() {
    let mut b = Buffer::mk_empty_buf(1,1);
    b.insert('a');
    b.insert('b');
    assert_eq!(b.data, vec!(vec!('a', 'b')));
}

#[test]
fn test_newline() {
    let mut b = Buffer::mk_empty_buf(10,10);
    assert_eq!(b.data.len(), 1);
    b.insert('a');
    b.add_newline();
    b.insert('b');
    assert_eq!(b.data.len(), 2);
}

#[test]
fn test_japanese() {
    let mut b = Buffer::mk_empty_buf(10,10);
    b.insert('並');
    assert_eq!(b.data,vec!(vec!('並')));
}
