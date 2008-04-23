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
import System.IO
import Data.Int

type InputTarContent = (Int64, FilePath)
type InputTarSize = (FilePath, Maybe Int64)

main :: IO ()
main = 
    do --updateGlobalLogger "" (setLevel INFO)
       argv <- getArgs
       (encoder, offsetfn) <- case argv of
                                [x, y] -> return (x, y)
                                _ -> usage
       
       inpData <- BSL.getContents

       inpcontent_str <- scanInput inpData
       let sizes = convToSize . parseMinusR $ inpcontent_str

       procData inpData sizes
       hPrint stderr (BSL.length inpData)

procData :: BSL.ByteString -> [InputTarSize] -> IO ()
procData = worker 0 
worker _ _ [] = return ()
worker bytesWritten inp (thisSize:xs) =
    case thisSize of
      (fp, Nothing) -> -- Last entry
          do write (BSL.length inp) fp
             BSL.putStr inp
      (fp, Just sz) -> -- Regular entry
          do let fullsize = sz * 512
             write fullsize fp
             let (thiswrite, remainder) = BSL.splitAt fullsize inp
             BSL.putStr thiswrite
             worker (bytesWritten + fullsize) remainder xs
    where write l fp =
              hPutStrLn stderr $ show bytesWritten ++ "\t" ++ show l ++ "\t" ++
                        fp

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
parseMinusR = map procLine . lines
    where procLine l = case parse entry ("Line: " ++ show l) l of
                         Left x -> error (show x)
                         Right y -> y
          entry = do string "block "
                     bn <- many1 digit
                     string ": "
                     fn <- many1 (noneOf "\n\r")
                     eof
                     return (read bn, fn)
          
convToSize :: [InputTarContent] -> [InputTarSize]
convToSize (i1:i2:xs) =
    (snd i1, Just (fst i2 - fst i1)) : convToSize (i2:xs)
convToSize [i1] = 
    [(snd i1, Nothing)]
convToSize [] = []
