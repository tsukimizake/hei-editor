module Lib where
import Foreign.C.Types
import Foreign.C.String

foreign import ccall unsafe "init_ncurses" initBackend :: IO ()
foreign import ccall unsafe "end_ncurses" killBackend :: IO ()
foreign import ccall unsafe "interpret_cmd" interpretCmd :: CWchar -> IO ()
