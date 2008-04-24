{- 
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
-}

{- |
   Module     : Main
   Copyright  : Copyright (C) 2008 John Goerzen
   License    : GNU GPL, version 3 or above; see COPYRIGHT for details

   Maintainer : John Goerzen <jgoerzen@complete.org>
   Stability  : provisional
   Portability: portable

Written by John Goerzen, jgoerzen\@complete.org

-}

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Text.ParserCombinators.Parsec
import System.Environment
import HSH
import System.IO
import Data.Int
import Data.IORef
import Control.Monad
import System.Posix.IO
import System.Posix.Process
import Control.Exception(evaluate)

type InputTarContent = (Int64, FilePath)
type InputTarSize = (FilePath, Maybe Int64)

main :: IO ()
main = 
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       datafn <- case argv of
                   [x] -> return x
                   _ -> usage

       runIO $ teeFIFOBS [datafn] -|- ("tar", ["-Rtf", "-"])

usage :: IO a           
usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenc-scanner tarfifoname"
       putStrLn "input from stdin, output blocks to stdout, and input copied"
       putStrLn "to given fifoname."
       fail "Usage error"
