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
module Commands where

import Text.Printf
import Utils

import qualified Commands.Cat
import qualified Commands.Encode
import qualified Commands.Scan

allCommands :: [(String, Command)]
allCommands = 
    [
     Commands.Cat.cmd,
     Commands.Encode.cmd,
     lscommands,
     Commands.Scan.cmd
    ]

lscommands :: (String, Command)
lscommands = 
    simpleCmd "lscommands" "Display a list of all available commands" ""
                  [] lscommands_worker

lscommands_worker :: ([(String, String)], [String]) -> IO ()
lscommands_worker _ =
    do putStrLn "\nAll available tarf commands:\n"
       printf "%-20s %s\n" "Name" "Description"
       putStrLn "-------------------- -------------------------------------------------------"
       mapM_ (\(_, x) -> printf "%-20s %s\n" (cmdname x) (cmddescrip x))
             allCommands
       putStrLn ""
