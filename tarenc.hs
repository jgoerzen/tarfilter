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
import System.Path

main :: IO ()
main = brackettmpdir "tarenc.XXXXXX" $ \tmpdir ->
    do --updateGlobalLogger "" (setLevel INFO)
      
       argv <- getArgs
       (encoder, offsetfn) <- case argv of
                                [x, y] -> return (x, y)
                                _ -> usage

       let fifopath = tmpdir ++ "/tarfifo"
       pn <- getProgName
       abspn <- abspath pn
       let pnbase = basename abspn
       
       runIO $ (pnbase ++ "/tarenc-scanner", [fifopath]) -|-
               (pnbase ++ "/tarenc-encoder", [fifopath, encoder, offsetfn])
           
usage :: IO a
usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenc encoder outputoffsetfilename"
       putStrLn "input from stdin, output tar file is written to stdout"
       putStrLn "use /dev/null for outputoffsetfilename if you don't want offset info"
       fail "Usage error"
