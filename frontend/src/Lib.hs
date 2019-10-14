module Lib where
import Foreign.C.Types
import Foreign.C.String

foreign import ccall unsafe "init_ncurses" initNC :: IO ()
foreign import ccall unsafe "interpret_cmd" interpretCmd :: CChar -> IO ()
