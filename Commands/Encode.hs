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
      "Encode a tar file with the Tarf algorithm" helptext
      [Option "e" ["encoder"] (ReqArg (stdRequired "e") "PROGRAM")
              "Program to use for encoding",
       Option "w" ["writeindex"] (ReqArg (stdRequired "w") "FILE")
              "Write non-encoded index to FILE"
      ] 
      cmd_worker

cmd_worker (args, []) =
    do encoder <- case lookup "e" args of
                    Just x -> return x
                    Nothing -> fail "encode: --encoder required; see tarf encode --help"
       indexfn <- case lookup "w" args of 
                    Just x -> return x
                    Nothing -> fail "encode: --writeindex required; see tarf encode --help"
       prog <- getProgram "tarf-encoder"
       safeSystem prog [encoder, indexfn]

cmd_worker _ =
    fail $ "Invalid arguments to encode; please see tarf encode --help"

helptext = 
    "Usage: tarf encode -e encoder -w /path/to/index < tar > tarf\n\n\
\Read a tar file from standard input.  Encode using the given encoder\n\
\into the tarf format.  Write the resulting tar file to stdout, and\n\
\write an index to the file given by -w.\n"
