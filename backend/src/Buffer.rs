
use super::Direction::{*};
use super::EditorError::{*};
use super::Pos::{*};
use std::fs::{*};
use std::io::Read;

use ncurses as nc;
pub struct Buffer {
  data : Vec<Vec<char>>,
  cursorpos : Pos
}

impl Buffer {
  pub fn mk_file_buf(filePath:&str) -> Result<Buffer,EditorError> { // unimplemented: make it lazy
    let f = File::open(filePath);
    let zeropos = Pos {x:0, y:0};
    return match f {
      Ok(mut f) => 
        {
          let mut dat = String::new();
          f.read_to_string(&mut dat).expect("could not read file as string"); // TODO: can elim copy using readline?
          let mut lines:Vec<Vec<char>>=Vec::new();
          for line in dat.split("\n") {
             lines.push(line.to_string().chars().collect());
          }
          Ok(Buffer{data:lines,cursorpos:zeropos})
        },
      Err(_e) => Err(EditorError{msg:format!("could not find file ${}", filePath)})
    }
  }

  pub fn mk_empty_buf() -> Buffer {
    let mut data : Vec<Vec<char>>= Vec::new();
    data.push(Vec::new()); // push first line
    let cursorpos = Pos {x:0, y:0};
    return Buffer{data, cursorpos}
  }
  pub fn insert(&mut self, c:char){
    let x = self.cursorpos.x as usize;
    let y = self.cursorpos.y as usize;
    self.data[y].insert(x, c);
    nc::mvaddch(self.cursorpos.y as i32, self.cursorpos.x as i32, c as u32);
    self.cursorpos.x = self.cursorpos.x + 1;

    // ???
    let mut line:&Vec<char> = &self.data[y];
  }
  pub fn redraw_all() {
    unimplemented!();
  }
  pub fn redraw_pos(&mut self, pos:Pos){

  }
  pub fn move_cursor(&mut self, direction:Direction, offset:i32){
    let x = self.cursorpos.x;
    let y = self.cursorpos.y;
    match direction {
        Direction::Horizontal => {
            if offset >= 0 {
                let len = if self.data[y as usize].len() == 0 {
                    0
                }else {
                    self.data[y as usize].len() as u32 - 1
                };
                self.cursorpos.x = std::cmp::max(x + offset as u32, len);
            } else{
                let len = if self.data[x as usize].len() == 0 {
                    0
                }else {
                    self.data[x as usize].len() as u32 - 1
                };
                self.cursorpos.x = std::cmp::min(x + offset as u32, 0);
            }
        },
        Direction::Vertical => {
            if offset >= 0 {
                self.cursorpos.y = std::cmp::max(y + offset as u32, self.data[y as usize].len() as u32 - 1);
            } else{
                self.cursorpos.y = std::cmp::min(y + offset as u32, 0);
            }
        }
    }
  }
  pub fn get_size()->Pos{
    unimplemented!();
  }
  pub fn save_file() {
    unimplemented!();
  }
  pub fn add_newline(&mut self){
      self.data.push(Vec::new());
      self.move_cursor(Direction::Vertical, 1);
  }
}
