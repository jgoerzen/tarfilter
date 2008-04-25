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

module Commands.Scan where
import System.Console.GetOpt
import System.Console.GetOpt.Utils
import System.Cmd.Utils
import Utils
import HSH
import System.IO

cmd = simpleCmd "scan"
      "Read a plain tar file and produce an index file for it" helptext
      []
      cmd_worker

cmd_worker ([], []) = bracketFIFO "tarf-scan.XXXXXX" $ \fifoname ->
    do hSetBuffering stdin (BlockBuffering (Just 10240))
       hSetBuffering stdout LineBuffering
       progname <- getProgram "tarf-encoder"
       runIO $ (progname, ["cat", fifoname]) -|-
               discard -|- catFromBS [fifoname]

cmd_worker _ =
    fail $ "Invalid arguments to scan; please see tarf scan --help"

helptext = 
    "Usage: tarf scan < tar > index\n\n\
\Read a plain tar file from standard input.  Write an index for this\n\
\uncompressed file to standard output.\n"
