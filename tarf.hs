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

import System.Log.Logger
import System.Log.Handler.Simple
import System.IO(stderr)
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Environment
import Data.List
import System.Exit
import Commands
import Control.Monad
import Utils

main = 
    do updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       let (optargs, commandargs) = span (isPrefixOf "-") argv
       case getOpt RequireOrder options optargs of
         (o, n, []) -> worker o n commandargs
         (_, _, errors) -> usageerror (concat errors) -- ++ usageInfo header options)
       
options = [Option "d" ["debug"] (NoArg ("d", "")) "Enable debugging",
           Option "" ["help"] (NoArg ("help", "")) "Display this help"]

worker args n commandargs =
    do when (lookup "help" args == Just "") $ usageerror ""
       when (lookup "d" args == Just "") 
            (updateGlobalLogger "" (setLevel DEBUG))
       handler <- streamHandler stderr DEBUG
       updateGlobalLogger "" (setHandlers [handler])
       let commandname = head cmdargs
       case lookup commandname allCommands of
         Just command -> 
             execcmd command (tail cmdargs)
         Nothing -> usageerror ("Invalid command name " ++ commandname)
       where cmdargs = case commandargs of
                         [] -> ["INVALID"]
                         x -> x

usageerror errormsg =
    do putStrLn errormsg
       putStrLn (usageInfo header options)
       putStrLn "Run \"tarf lscommands\" for a list of available commands.\n\
                \Run \"tarf command --help\" for help on a particular command.\n"
       exitFailure

header = "Usage: tarf [global-options] command [command-options]\n\n\
         \Available global-options are:\n"
