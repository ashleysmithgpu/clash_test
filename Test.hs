
module Main where

import System.Environment
import Clash.Prelude
import Text.Read
import Data.Maybe
import System.IO
import FloatAdder

main :: IO ()
main = do
    end <- isEOF
    if end
        then putStr ""
        else do
            line1 <- getLine
            let a = readMaybe line1 :: Maybe Integer
            line2 <- getLine
            let b = readMaybe line2 :: Maybe Integer
            case a of
                Just x -> case b of
                    Just y -> do
                        hFlush stdout
                        print(toInteger (f32add1 (fromInteger (toInteger x), fromInteger (toInteger y))))
                        hFlush stdout
                    Nothing -> putStrLn "Fuck"
                Nothing -> putStrLn "Fuck"
            main
