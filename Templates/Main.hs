module Main where

import Control.Monad.Except
import Data.Char
import Lib
import System.Environment
import System.IO

data Exp = EInt Int | EAdd Exp Exp | ESub Exp Exp | EMul Exp Exp

instance Num Exp where
  fromInteger = EInt . fromInteger -- The second fromInteger has typed arguments
  a + b = EAdd a b
  a - b = ESub a b
  a * b = EMul a b
  negate a = EInt (-1) * a
  abs = undefined
  signum = undefined

testExp :: Exp
testExp = 3

simple :: Exp -> Exp
simple (EMul (EInt 0) _) = EInt 0
simple (EMul (EInt 1) t1) = simple t1
simple (EAdd (EInt 0) x) = simple x
simple (EAdd x y) = simple $ EAdd (simple x) (simple y)
simple t1 = t1

--------------------------

readInts :: String -> [Int]
readInts xs = do
  s <- words xs
  return $ read s

readIntsBind :: String -> [Int]
readIntsBind xs = read <$> words xs

type Result = Either String

readAllInts :: String -> [Result Int]
readAllInts xs = do
  s <- words xs
  return $ readOneInt s

readInts2 :: String -> Result [Int]
readInts2 xs = sequence $ readAllInts xs

readOneInt :: String -> Result Int
readOneInt s
  | all isDigit s = return $ read s
  | otherwise = throwError ("NAN: " ++ s)

------------------------------------

--main :: IO ()
--main = someFunc

-- a
printArgs :: IO ()
printArgs = getArgs >>= printIO

printIO :: [String] -> IO ()
printIO = foldr ((>>) . putStrLn) (putStrLn "End.")

--b
askForHaskell :: IO ()
askForHaskell = do
  putStrLn "What is your favorite language?"
  s <- getLine
  case map toLower s of
    "haskell" -> return ()
    _ -> askForHaskell

askForHaskell2 = while test askForHaskell2
  where
    test = do
      putStrLn "What is your favorite language?"
      s <- getLine
      return $ s == "haskell"

while test block = test >>= (\u -> if u then return () else block)

--c

main = do
  args <- getArgs
  case args of
    [file] -> openFile file ReadMode >>= wc
    [] -> wc stdin
    _ -> do
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " file"

wc :: Handle -> IO ()
wc fileHandle = do
  text <- hGetContents fileHandle
  let result = [length text, length $ words text, length $ lines text]
  putStrLn $ unwords $ map show result

