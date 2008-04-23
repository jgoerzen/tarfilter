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
import Text.ParserCombinators.Parsec

type InputTarContent = (Integer, FilePath)

main :: IO ()
main = 
    do updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       (encoder, offsetfn) <- case argv of
                                [x, y] -> return (x, y)
                                _ -> usage
       
       inpData <- BSL.getContents
       
       inpcontent_str <- scanInput inpfn
       let inpcontent_parsed = parseMinusR inpcontent_str
       print inpcontent

usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenv encoder outputoffsetfilename"
       putStrLn "input from stdin, output tar file is written to stdout"
       putStrLn "use /dev/null for outputoffsetfilename if you don't want offset info"
       
scanInput :: BSL.ByteString -> String
scanInput inp =
    runIO $ echoBS inp -|- ("tar", ["-Rtf", fp])

parseMinusR :: String -> InputTarContent
parseMinusR inp =
    case parse outFile "(stdin)" inp of
      Left x -> fail (show x)
      Right y -> y
    where outFile = do r <- many1 entry
                       eof
                       return r
          entry = do string "block "
                     bn <- many1 digit
                     string ": "
                     fn <- many1 (noneOf "\n\r")
                     eol
                     return (read bn, fn)
          eol = try (string "\r\n" <|> string "\n" <|> string "\r")
          
          