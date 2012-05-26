module Main where

import qualified Data.ByteString.Char8 as BS
import Data.List (intercalate)

import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)

import Text.Format (scanFormatString)

fmtstr = "$remote_addr - $remote_user [$time_local] [took $request_time ms] \"$request\" "

main = do
  args <- getArgs
  let (_, cio) = case args of
                  [] -> ("INPUT", return . BS.pack =<< getContents)
                  [fn] -> (fn, BS.readFile fn)
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
  mapM_ (out . scanFormatString fmtstr . BS.unpack) $ BS.lines c
  where
    out (Left s) = putStrLn s
    out (Right v) = print v

