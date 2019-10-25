#[macro_use]
extern crate lazy_static;
extern crate ncurses;
use ncurses as nc;
mod buffer;
mod direction;
mod editor_error;
mod pos;

lazy_static! {
    static ref BUF: std::sync::RwLock<buffer::Buffer> = {
        let b = buffer::Buffer::mk_empty_buf(0,0);
        std::sync::RwLock::new(b)
    };
}

#[no_mangle]
pub extern "C" fn init_ncurses() -> () {
    nc::setlocale(nc::LcCategory::all, "");
    nc::initscr();
    nc::keypad(nc::stdscr(), true);
    nc::noecho();
}

#[no_mangle]
pub extern "C" fn end_ncurses() -> () {
    nc::endwin();
}

#[no_mangle]
pub extern "C" fn interpret_cmd(cmd: char) -> () {
    match cmd {
        '\n' => BUF.write().unwrap().add_newline(),
        _ => BUF.write().unwrap().insert(cmd),
    }
    BUF.read().unwrap().redraw_all();
}

#[no_mangle]
pub extern "C" fn save_file() -> () {
    BUF.read().unwrap().save_file();
}

