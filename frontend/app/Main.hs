{-# LANGUAGE Strict #-}
module Main where

import Lib
import Control.Monad
import Foreign.C.Types
import Data.Char

main :: IO ()
main = do 
  initBackend
  forever $ do 
    input <- getChar
    interpretCmd $ CWchar . fromIntegral . ord $ input

