#[macro_use]
extern crate lazy_static;
extern crate ncurses;
use ncurses as nc;
mod Pos;
mod Direction;
mod Buffer;
mod EditorError;

lazy_static!{
    static ref buf:std::sync::RwLock<Buffer::Buffer> = {
        init_ncurses();
        let b = Buffer::Buffer::mk_empty_buf();
        std::sync::RwLock::new(b)
    };
}

fn read_input(c:char){
    unimplemented!();
}

#[no_mangle]
pub extern fn init_ncurses() -> () {
  nc::setlocale(nc::LcCategory::all,"");
  nc::initscr();
  nc::keypad(nc::stdscr(), true);
  nc::noecho();
}

#[no_mangle]
pub extern fn interpret_cmd(cmd:char) -> () {
    match cmd {
        '\n' => {buf.write().unwrap().add_newline()},
        _ => buf.write().unwrap().insert(cmd)
    }
}

