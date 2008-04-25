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

module Commands.Encode where
import System.Console.GetOpt
import System.Console.GetOpt.Utils
import System.Cmd.Utils
import Utils

cmd = simpleCmd "encode"
      "Encode a plain tar file with the Tarf algorithm" helptext
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
                    Nothing -> fail "decode: --decoder required; see tarf decode --help"
       indexfn <- case lookup "r" args of 
                    Just x -> return x
                    Nothing -> fail "decode: --readindex required; see tarf decode --help"
       tardatah <- case lookup "f" args of
                     Just x -> openFile x ReadMode
                     Nothing -> stdin
       
       tarindexstr <- readFile indexfn
       
       prog <- getProgram "tarf-encoder"
       safeSystem prog [encoder, indexfn]

cmd_worker _ =
    fail $ "Invalid arguments to decode; please see tarf decode --help"

helptext = 
    "Usage: tarf decode -d decoder -r /path/to/index -f file > tar\n\n\
\Read a tarf-formatted file from standard input.  Using the index given\n\
\by -r
\Read a tar file from standard input.  Encode using the given encoder\n\
\into the tarf format.  Write the resulting tar file to stdout, and\n\
\write an index to the file given by -w.\n"
