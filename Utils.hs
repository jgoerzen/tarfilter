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
module Utils where
import System.Console.GetOpt.Utils
import System.Console.GetOpt
import System.Exit
import System.Environment
import HSH
import System.Path
import System.Posix.Files(createNamedPipe)

data Command =
             Command {cmdname :: String,
                      cmddescrip :: String,
                      execcmd :: [String] -> IO ()}

simpleCmd :: 
          String -> String -> String -> [OptDescr (String, String)] 
          -> (([(String, String)], [String]) -> IO ()) 
          -> (String, Command)
simpleCmd name descrip helptext optionsinp func =
    (name, Command {cmdname = name, cmddescrip = descrip,
                    execcmd = worker})
    where options =
              optionsinp ++ [Option "" ["help"] (NoArg ("help", "")) "Display this help"]
          worker argv =
              case getOpt RequireOrder options argv of
                (o, n, []) -> 
                    if (lookup "help" o == Just "") 
                       then usageerror []
                       else func (o, n)
                (_, _, errors) -> usageerror (concat errors)
          usageerror errormsg =
              do putStrLn $ "Error processing arguments for command " ++ 
                          name ++ ":"
                 putStrLn errormsg
                 putStrLn (usageInfo header options)
                 putStrLn helptext
                 exitFailure
          header = "Available command-options for " ++ name ++ " are:\n"

getProgram :: String -> IO String
getProgram x =
    do pn <- getProgName
       abspn <- abspath pn
       return (dirname abspn ++ "/" ++ x)

bracketFIFO :: String -> (String -> IO a) -> IO a
bracketFIFO pattern func =
    brackettmpdir pattern fifofunc
    where fifofunc dirname =
              let fifoname = dirname ++ "/" ++ "fifo"
                  in do createNamedPipe fifoname 0o600
                        func fifoname
