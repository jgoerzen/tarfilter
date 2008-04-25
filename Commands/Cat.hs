{- 
Copyright (C) 2008 John Goerzen <jgoerzen@complete.org>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Commands.Cat where
import System.Console.GetOpt
import System.Console.GetOpt.Utils
import System.Cmd.Utils
import Utils

cmd = simpleCmd "cat"
      "Decode a Tarf-encoded tar file, output plain tar file" helptext
      [Option "d" ["decoder"] (ReqArg (stdRequired "d") "PROGRAM")
              "Program to use for decoding",
       Option "r" ["readindex"] (ReqArg (stdRequired "r") "FILE")
              "Read non-incoded index for input from FILE",
       Option "f" ["file"] (ReqArg (stdRequired "f") "FILE")
              "Read the tarf-formatted tar file from FILE\nIf -f is not given, read from stdin"
      ] 
      cmd_worker

cmd_worker (args, []) =
    do decoder <- case lookup "d" args of
                    Just x -> return x
                    Nothing -> fail "tarf cat: --decoder required; see tarf decode --help"
       indexfn <- case lookup "r" args of 
                    Just x -> return x
                    Nothing -> fail "tarf cat: --readindex required; see tarf decode --help"
       tardatah <- case lookup "f" args of
                     Just x -> openFile x ReadMode
                     Nothing -> stdin
       
       tarindexstr <- readFile indexfn
       let tarindex' = parseIndex tarindexstr

       let nullblock = last tarindex'
       let tarindex = init tarindex'
       
       seekable <- hIsSeekable tardatah
       let seekfunc =
               case seekable of
                 True -> (\h -> hSeek h RelativeSeek)
                 Fasle -> hSkip

       processTar decoder tardatah seekfunc (tarindex ++ [nullblock])

cmd_worker _ =
    fail $ "Invalid arguments to cat; please see tarf cat --help"

processTar :: String -> Handle -> (Integer -> IO ()) -> TarFile -> IO ()
processTar decoder tardatah seekfunc tf = 
    do (cmpoffset, offset) <- processTar' decoder tardatah seekfunc 0 0 tf
       -- Make sure we pad with nulls to next 10240-byte boundary
       let neededPad = (fromImtegral) (offset `mod` 10240)
       let nulls = replicate neededPad 0
       BSL.putStr (BSL.pack nulls)

processTar' :: String -> Handle -> (Integer -> IO ()) -> Integer -> Integer -> TarFile -> IO (Integer, Integer)
processTar' _ _ _ cmpoffset offset [] = return (cmpoffset, offset)
processTar' decoder tardatah seekfunc cmpoffset offset (te:xs) = 
    do skipIt
       runIO $ catBytesFrom 4096 tardatah
                 (Just (cmpSize te)) -|- decoder
       processTar' decoder tardatah seekfunc newCmpOffset newOffset xs
    where skipAmountCmp = cmpOff te - cmpoffset
          newCmpOffset = cmpoffset + skipAmountCmp + cmpSize te
          newOffset = offset + uncSize te
          skipIt 
              | skipAmountCmp == 0 = return ()
              | otherwise = seekfunc skipAmountCmp

helptext = 
    "Usage: tarf cat -d cat -r /path/to/index -f file > tar\n\n\
\Read a tarf-formatted file from standard input.  Using the index given\n\
\by -r, decode the file, using efficient seeks if supported by the\n\
\underlying input supply.  Write resulting plain tar file to stdout.\n\
\\n\
\If -f is specified, read from the named file instead of stdin.\n"

