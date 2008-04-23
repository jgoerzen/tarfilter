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
import System.Environment
import HSH

type InputTarContent = (Integer, FilePath)
type InputTarSize = (FilePath, Maybe Integer)

main :: IO ()
main = 
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       (encoder, offsetfn) <- case argv of
                                [x, y] -> return (x, y)
                                _ -> usage
       
       inpData <- BSL.getContents
       
       inpcontent_str <- scanInput inpData
       let inpcontent_parsed = parseMinusR inpcontent_str
       print inpcontent_parsed
       let inpcontent_sizes = convToSize inpcontent_parsed
       print inpcontent_sizes

usage =
    do putStrLn "Usage:\n"
       putStrLn "tarenc encoder outputoffsetfilename"
       putStrLn "input from stdin, output tar file is written to stdout"
       putStrLn "use /dev/null for outputoffsetfilename if you don't want offset info"
       fail "Usage error"
       
scanInput :: BSL.ByteString -> IO String
scanInput inp =
    run $ echoBS inp -|- ("tar", ["-Rtf", "-"])

parseMinusR :: String -> [InputTarContent]
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
          
convToSize :: [InputTarContent] -> [InputTarSize]
convToSize (i1:i2:xs) =
    (snd i1, Just (fst i2 - fst i1)) : convToSize (i2:xs)
convToSize [i1] = 
    [(snd i1, Nothing)]
convToSize [] = []
