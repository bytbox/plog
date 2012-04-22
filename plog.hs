module Main where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as M

import System.Environment
import System.Exit (exitFailure)
import System.IO (openFile, hGetContents, hClose)

import Text.Parsec
import Text.Parsec.ByteString

data LogEntry = LogEntry
  { http        :: Map String String
  , sent_http   :: Map String String
  , remote_addr :: String
  }
  deriving Show

emptyLogEntry = LogEntry
  { http        = M.fromList []
  , sent_http   = M.fromList []
  , remote_addr = ""
  }

type Transform = LogEntry -> LogEntry

simple_string :: Parsec String () String
simple_string = many $ noneOf [' ', '\t', '\n']

string_between :: Char -> Char -> Parsec String () String
string_between a b = between (char a) (char b) $ many $ noneOf [b]

integer = do
  ds <- many1 digit
  return 0

parse_remote_addr = do
                      s <- simple_string
                      return $ \x -> x {remote_addr = s}

idt p = p >> return id
raw_string = idt . string
any_string = idt simple_string

format :: [Parsec String () Transform]
format =
  [ parse_remote_addr
  , raw_string "-"
  , any_string
  , idt $ string_between '[' ']'
  , idt $ string_between '[' ']'
  , idt $ string_between '"' '"'
  , idt $ integer
  , idt $ integer
  , idt $ string_between '"' '"'
  , idt $ string_between '"' '"'
  , idt $ string_between '"' '"'
  ]

logline :: Parsec String () Transform
logline = foldl (\t f -> f `composeTransform` t) (return id) format
  where composeTransform a b = do
                                y <- b
                                skipMany (oneOf [' ', '\t'])
                                x <- a
                                return $ x . y

parser :: Parsec String () [LogEntry]
parser = do
  ts <- many $ do
                t <- logline
                newline
                return $ t emptyLogEntry
  eof
  return ts

main = do
  args <- getArgs
  let (f, cio) = case args of
                  [] -> ("INPUT", getContents)
                  [fn] -> (fn, readFile fn)
                  _ -> ("", do
                              pn <- getProgName
                              putStrLn $ intercalate " "
                                [ "usage:"
                                , pn
                                , "[filename]"
                                ]
                              exitFailure
                        )
  c <- cio

  case runParser parser () f c of
    Left x -> putStrLn $ show x
    Right les -> putStrLn $ show $ map remote_addr $ les

