module Main where

import Lib
import Control.Monad
import Foreign.C.Types
import Data.Char

main :: IO ()
main = do 
  initBackend
  input <- getChar
  forever $ interpretCmd $ CWchar . fromIntegral . ord $ input

