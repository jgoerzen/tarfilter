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

import System.Environment
import HSH
import System.IO
import Control.Monad
import System.Path(brackettmpdir)
import System.Posix.Files(createNamedPipe)

main :: IO ()
main = brackettmpdir "tarenc-scanner.XXXXXX" $ \tmpdir ->
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       blocksfn <- case argv of
                   [x] -> return x
                   _ -> usage

       {- We use tee to send our raw tar data to stdout and tarfifo.
          Then we can fire up a second process that uses tar to copy
          from that fifo to blocksfn. -}

       let tarfifofn = tmpdir ++ "/scannerfifo"
       createNamedPipe tarfifofn 0o600

       cmd1r <- run $ teeFIFOBS [tarfifofn]
       cmd2r <- run $ ("cat", [tarfifofn]) -|- 
                ("tar", ["-Rtf", "-"]) -|- catToFIFOBS blocksfn

       cmd1r >>= checkResults
       cmd2r >>= checkResults
       
usage :: IO a           
usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenc-scanner tarfifoname"
       putStrLn "input from stdin, output blocks to tarfifoname, and input copied"
       putStrLn "to stdout."
       fail "Usage error"
